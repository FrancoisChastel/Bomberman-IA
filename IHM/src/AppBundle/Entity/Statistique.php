<?php

namespace AppBundle\Entity;

use Doctrine\ORM\Mapping as ORM;

/**
 * Statistique
 *
 * @ORM\Table(name="statistique")
 * @ORM\Entity(repositoryClass="AppBundle\Repository\StatistiqueRepository")
 */
class Statistique
{
    /**
     * @var int
     *
     * @ORM\Column(name="id", type="integer")
     * @ORM\Id
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    private $id;

    /**
     * @var string
     *
     * @ORM\Column(name="J1", type="string", length=255)
     */
    private $j1;

    /**
     * @var string
     *
     * @ORM\Column(name="J2", type="string", length=255)
     */
    private $j2;

    /**
     * @var string
     *
     * @ORM\Column(name="J3", type="string", length=255, nullable=true)
     */
    private $j3;

    /**
     * @var string
     *
     * @ORM\Column(name="J4", type="string", length=255, nullable=true)
     */
    private $j4;

    /**
     * @var string
     *
     * @ORM\Column(name="winner", type="string", length=255)
     */
    private $winner;


    /**
     * Get id
     *
     * @return int
     */
    public function getId()
    {
        return $this->id;
    }

    /**
     * Set j1
     *
     * @param string $j1
     *
     * @return Statistique
     */
    public function setJ1($j1)
    {
        $this->j1 = $j1;

        return $this;
    }

    /**
     * Get j1
     *
     * @return string
     */
    public function getJ1()
    {
        return $this->j1;
    }

    /**
     * Set j2
     *
     * @param string $j2
     *
     * @return Statistique
     */
    public function setJ2($j2)
    {
        $this->j2 = $j2;

        return $this;
    }

    /**
     * Get j2
     *
     * @return string
     */
    public function getJ2()
    {
        return $this->j2;
    }

    /**
     * Set j3
     *
     * @param string $j3
     *
     * @return Statistique
     */
    public function setJ3($j3)
    {
        $this->j3 = $j3;

        return $this;
    }

    /**
     * Get j3
     *
     * @return string
     */
    public function getJ3()
    {
        return $this->j3;
    }

    /**
     * Set j4
     *
     * @param string $j4
     *
     * @return Statistique
     */
    public function setJ4($j4)
    {
        $this->j4 = $j4;

        return $this;
    }

    /**
     * Get j4
     *
     * @return string
     */
    public function getJ4()
    {
        return $this->j4;
    }

    /**
     * Set winner
     *
     * @param string $winner
     *
     * @return Statistique
     */
    public function setWinner($winner)
    {
        $this->winner = $winner;

        return $this;
    }

    /**
     * Get winner
     *
     * @return string
     */
    public function getWinner()
    {
        return $this->winner;
    }
}

