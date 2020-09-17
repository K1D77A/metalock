# metalock

This is a project simply provides a metaclass which when used means that you do not have to manually 
manage locks. This package exports one thing `metalock` which is a metaclass for use with whatever
class definitions you want. This metaclass is implemented using *-using-class methods so your
distribution of CL must use these methods to implement access functions like 'slot-value' for this metaclass to work. Obviously it must also implement the MOP. I have tested on SBCL 1.4.9 only.

Access to slots is controlled using read-writer locks described here: 
https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock#Using_a_condition_variable_and_a_mutex


![Example of it in action:](https://i.imgur.com/7NybNvC.png)

## License

MIT

* needs to work for initialized instances when you redefine the class.
