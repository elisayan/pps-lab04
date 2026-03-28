package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*
import SchoolModel.BasicSchoolModule.*
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*

class SchoolModelTest:

  @Test def testEmptySchool() =
    val school = emptySchool

    assertEquals(Nil(), school.teachers)
    assertEquals(Nil(), school.courses)
    assertFalse(school.hasTeacher("John"))
    assertFalse(school.hasCourse("Math"))

  @Test def testSingleAssignment() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")

    val school2 = school.setTeacherToCourse(john, math)

    assertEquals(Cons("John", Nil()), school2.teachers)
    assertEquals(Cons("Math", Nil()), school2.courses)
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))

  @Test def testMultipleCoursesSameTeacher() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")

    val school3 = school
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)

    assertEquals(Cons("John", Nil()), school3.teachers)
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.courses)
    assertTrue(school3.hasTeacher("John"))
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))

  @Test def testCoursesOfATeacher() =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")

    val school3 = school
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)

    assertEquals(
      Cons(math, Cons(italian, Nil())),
      school3.coursesOfATeacher(john)
    )