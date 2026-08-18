# The Algorithm Takes the Stand

Asked what his lure is supposed to be, the man who made it says he was thinking of a cluster of small fish, and then, in the same breath, tells you not to take that seriously. What matters is whether a bass strikes, and bass, Hideo Yoshida writes, react in ways that defy our imagination. He has been making the things for twenty years. The spiky plastic ball is now the most sought-after bait in American freshwater fishing, cleared off shelves, resold for many times retail. It resembles nothing that lives in the water. The designer does not claim to know why it works.

The disclaimer is easy to read as modesty. It isn't. It's an accurate report on how the object came to exist. Yoshida made variants; the market and the fish selected; two decades later the survivor is a shape no one would have derived from a theory of bass cognition, because no one held one. The confirmation is in the imitation — Rapala, Strike King, Yamamoto, and Berkley have all pushed out urchin-style baits, which is convergence by several firms on a form none of them can account for either.

Be precise about what is missing here, because the imprecise version of this observation is about to become a legal defense. Yoshida is not absent. He ran the loop: he chose to make variants, chose which to keep, chose what counted as success. What is absent is narrower and stranger — no one authored the *particular feature* that makes the thing work, and no one holds a theory adequate to explain it. The loop is fully attributable to the person who ran it. Its output is explicable by nobody.

That distinction is the whole argument, and losing it in either direction wrecks the analysis. Collapse it one way and you get mysticism: the machine did it, nobody is responsible. Collapse it the other and you get the demand that someone produce a memo that was never written.

---

The default account of engineered harm runs on the memo. Call its author the technician: someone with a theory of your nervous system and a product line to apply it to. He is not a paranoid invention. Tobacco is the cleanest case — the firm understood the mechanism, documented that it understood, and optimized around it, and the 1998 settlement was possible because those documents existed. The technician knew, wrote down what he knew, and the writing became the case.

The mistake is not looking for him. The mistake is treating the memo as a necessary condition rather than a sufficient one. A technician with a theory will produce a hyper-optimized artifact. So will variation, a metric, and culling, with no one holding the theory at any point. The two paths converge on the same object and diverge completely in what they leave behind for an investigator.

I assumed this meant our legal machinery — fraud requiring knowledge, deception requiring a mental state — was blind to the second path. That was wrong, and the way it is wrong turned out to be the essay.

---

The law has met this before, in a case about a bottle.

In *Escola v. Coca-Cola Bottling Co.* (1944), a waitress was injured when a bottle exploded and she could not show what the bottler had done wrong. Justice Traynor's concurrence argued that requiring proof of negligence in mass production was hopeless in a way that ought to be dispositive: the manufacturer controls the process, the plaintiff cannot see inside it, the manufacturer can spread or reduce the risk. That reasoning became strict products liability.

What *Escola* supplies is not a solution to the algorithmic case — physical-product doctrine does not simply migrate to software, and whether a recommender is a product, a service, or expression is exactly what is being fought over. What it supplies is a grammar: where a firm operates a system, outsiders cannot inspect its operative process, and the firm can adjust the resulting risk, a culpable mind is not the only available gateway to responsibility.

That grammar is in active use. The consolidated federal social-media litigation before Judge Yvonne Gonzalez Rogers is captioned *In re Social Media Adolescent Addiction/Personal Injury Products Liability Litigation*; plaintiffs are running design-defect theory against recommender architecture. Whether it works is unresolved. But the gap I thought I had found is not a gap. It is a classification dispute, and it is live.

---

The stranger thing happened on the way to that dispute.

In *Anderson v. TikTok* (3d Cir. 2024), a ten-year-old died after the For You feed served her videos of a self-asphyxiation challenge she had never searched for. The district court dismissed under Section 230. The Third Circuit reversed, holding — drawing on the Supreme Court's *Moody v. NetChoice* decision from earlier that year — that a platform's algorithmic curation is its own expressive activity, first-party speech rather than a third party's. Section 230 shields third-party content. So it does not shield this.

Notice that the court was not being clever. It was cornered. Section 230 offers two slots: content provided by another, or the platform's own conduct. There is no third slot for emergent curation that no one at the company could narrate. Any process that is neither purely someone else's speech nor purely a human editor's judgment gets forced onto one horn. The court took the horn that let the case proceed.

And that horn carries the First Amendment with it. Editorial selection has long been treated as protected expression; *Moody* extended the intuition to platform curation. So the doctrine already had a speaker-shaped slot waiting, and the algorithm was pushed into it. This is one finding with two faces. It is a sword in the products-liability docket — you cannot hide behind third-party content when the curation is yours. It is a shield in the regulatory docket — you cannot be told what to say. The firm picks which docket it is standing in.

RealPage is standing in the second. New York enacted the first state ban on algorithmic rent-setting, prohibiting landlords from adjusting rents on the recommendation of software drawing on pooled landlord data — a pure process rule with no mental-state element anywhere in it. Days after settling with the Justice Department in November 2025, RealPage sued the New York Attorney General in the Southern District of New York, arguing the statute is a content-, viewpoint-, and speaker-based restriction on its right to offer advice. The Attorney General moved to dismiss. At this writing the court has not ruled and the statute is in force.

---

Here the essay has to state a position rather than a diagnosis, because the two attributions can be pulled apart and nothing yet requires them to travel together.

Attribution for liability says: the firm answers for the system's effects because it built, trained, deployed, and profits from it, and can change it. Attribution for expression says: the system's output is the firm's protected speech. Products liability is the standing proof that the first does not entail the second — a manufacturer's product is attributable to it for loss allocation without anyone calling the product the manufacturer's speech. Antitrust and consumer protection can likewise reach conduct — data pooling, automatic implementation, coercive defaults, suppression of deviation — without reaching the communicative content of a recommendation.

So the danger is not that law will fail to find someone to hold responsible. It is that the vocabulary it reaches for to do so — authorship, editorial judgment, corporate expression — converts a basis for accountability into a claim of immunity, and that this conversion is happening case by case without anyone deciding it should.

---

The objection I take most seriously is that all of this is a gift to the guilty.

"Nobody designed it; the system produced it" is exactly the defense a culpable firm mounts. Purdue had documents. Meta had internal research it declined to publish. A framework redirecting attention from intent toward emergent process arrives pre-adapted for use as a laundering script, and will be used that way by people who have read none of it and will cite all of it.

Three things hold against that. Intentionality is evidence of responsibility, not the only possible basis for it — which is what *Escola* established. Where documents exist they should be used without mercy; the technician is not a myth and he has not retired. And emergence is not a magic word: someone still chose the objective. There is no loop without a metric, and no metric without a human who set it. The machines have not stopped keeping notes. They keep enormous notes — experiment reports, evaluation dashboards, A/B results. They have stopped keeping them in prose.

---

Which points at the constructive move, and at what it is missing.

The Justice Department's proposed consent decree with RealPage is instructive for what it declines to ask. It does not ask what RealPage knew, or what the model selected for. It constrains what the loop may ingest and how tightly its output is wired to action: training limited to backward-looking nonpublic data at least twelve months old, excluding price and geographically localized data; restrictions on features that auto-implement recommendations or discourage users from rejecting them.

Each constraint does identifiable work. The data-age floor breaks the feedback loop in which a competitor's current behavior becomes a live coordination signal. Excluding localized pricing data stops the recommendation from functioning as a near-real-time substitute for talking to a competitor. The limits on auto-implementation restore a point at which a human can depart from the model, and the limits on discouraging rejection keep that point from decaying into a ceremonial click.

Call the register diet and coupling: what the loop is allowed to eat, and how directly its output becomes action. It requires neither a mind to read nor an interior to interpret, which is why it fits the authorless case, and why it does not invite the reclassification — a pipeline with valves is harder to characterize as a speaker with a message than an output is.

Two caveats it would be dishonest to omit. The decree is a negotiated settlement, not an adjudicated model; it has survived no constitutional review, and the instrument that *has* survived is the weakest one available. In *National Retail Federation v. James* (S.D.N.Y., October 2025), Judge Rakoff upheld New York's requirement that sellers disclose when a price was set by an algorithm using the buyer's personal data, under the *Zauderer* line permitting compelled factual disclosure. You can be made to say an algorithm set the price. Whether you can be stopped from using one is what *RealPage v. James* will decide.

And diet and coupling are necessary, not sufficient. A system fed clean, stale, aggregated data and optimized for the wrong thing still produces the harm. Human review of recommendations generated to maximize revenue per tenant will mostly ratify them. The objective function is the closest thing to intent that an authorless system contains, and it is the one part of the loop a person unambiguously chose. A regulatory scheme that never looks at it has left the lever alone.

---

The boundary is fentanyl, which I originally used as the opening case and have demoted, because I was letting its vividness carry an argument it cannot bear.

I previously wrote that fentanyl won on logistics rather than attractiveness, and that people buying test strips to detect and decline it was clean evidence. It is not clean. A 2017 survey of street opioid users in Baltimore, Boston, and Providence found 27 percent preferring fentanyl; among people entering buprenorphine treatment with fentanyl-positive toxicology, a slight majority preferred it; a New York injection cohort ran strongly the other way, with 83 percent toxicological exposure against 18 percent intentional use — but the same researchers noted rising tolerance, which pushes preference toward fentanyl over time. Preference is endogenous to exposure. Revealed-preference reasoning is calibrated to uncompelled choice among known options, and that is not this market.

The framework does not reach it, and the reason is precise rather than sweeping. Other instruments exist — criminal law, precursor control, harm reduction, claims against licensed distributors. What does not exist is the condition every instrument above depends on: an identifiable operator running a loop it wants to keep running, with inputs that can be constrained and an interface where a human might decline. Process regulation grips an optimization loop only when someone is holding it.

---

The essay I set out to write claimed the machines that harm us have stopped keeping notes. They have stopped keeping the kind we know how to read. But I was wrong about what follows, because I assumed a court confronted with an absent author would fail to proceed. It does not fail. It locates agency in the firm that built, trained, deployed, and monetized the system — and in *Anderson* that location took the form of speech: the feed was TikTok's own expression, not a neutral conduit for someone else's.

The risk is not that the law will find no one to hold responsible. It is that the only vocabulary available for holding someone responsible is the vocabulary of expression, and that the conduct we wanted to regulate will therefore arrive in court wearing the Constitution.
