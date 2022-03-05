use rustc_hash::FxHashMap;
use std::mem;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct StrId(u32);

/// A simple (but apparently still fast) string interner based off the one from 
/// [here](https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html).
#[derive(Debug, Default)]
pub struct Interner {
    /// Map strings to location in vec
    map: FxHashMap<&'static str, StrId>,
    /// Vector holding all interned strings
    vec: Vec<&'static str>,
    /// Current active buffer.
    current_buf: String,
    /// Previous buffers (I think).
    bufs: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: FxHashMap::default(),
            vec: Vec::new(),
            current_buf: String::with_capacity(cap),
            bufs: Vec::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&id) = self.map.get(name) {
            return id;
        }

        let name = unsafe { self.alloc(name) };
        let id = StrId(self.vec.len() as u32);
        self.map.insert(name, id);
        self.vec.push(name);

        id
    }

    pub fn get(&mut self, index: StrId) -> &str {
        self.vec[index.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let capacity = self.current_buf.capacity();

        if capacity < self.current_buf.len() + name.len() {
            let new_capacity = (capacity.max(name.len()) + 1).next_power_of_two();
            let new_buffer = String::with_capacity(new_capacity);
            let old_buffer = mem::replace(&mut self.current_buf, new_buffer);
            self.bufs.push(old_buffer);
        }

        let interned = {
            let start = self.current_buf.len();
            self.current_buf.push_str(name);
            &self.current_buf[start..]
        };

        &*(interned as *const str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let mut interner = Interner::default();
        let a = interner.intern("Hello");
        let b = interner.intern("World");
        let c = interner.intern("World");
        interner.intern("Radish");
        interner.intern("Cats");
        interner.intern("Dakion");

        assert_eq!(interner.vec.len(), 5);
        assert_eq!(a, StrId(0));
        assert_eq!(b, c);

        let a_value = interner.get(a);

        assert_eq!(a_value, "Hello");

        println!("{:?}", interner);
    }
}