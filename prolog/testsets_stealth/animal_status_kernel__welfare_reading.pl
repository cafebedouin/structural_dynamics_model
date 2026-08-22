% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Welfare-Constrained Animal Use Regime (Welfare Reading of the Animal-Status Kernel)
 *   domain: moral philosophy/animal ethics/legal theory
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the contested
 *   animal-status kernel: animals are sentient, their suffering morally
 *   relevant, use remains lawful provided it is regulated to minimize pain,
 *   and property status is retained but burdened with welfare obligations.
 *   The standing arrangement under contest — and the referent of every metric
 *   here — is the actual regime of welfare-regulated animal use (statutory
 *   treatment floors, inspection, certification), assessed by this reading's
 *   own lights: the reading endorses the arrangement's legitimacy while
 *   counting the suffering and death the arrangement permits as real moral
 *   costs. Constraint-family decomposition: the same kernel yields three
 *   structurally distinct constraints — the property reading (animals outside
 *   the victim set; negligible extraction by its lights), this welfare
 *   reading (partial inclusion via suffering-capacity; moderate extraction),
 *   and the abolitionist reading (full inclusion as persons; maximal
 *   extraction, since property status itself is the injury). The epsilon
 *   values differ because the victim-set boundary differs; they are separate
 *   files linked through network.affects_constraints, not one constraint
 *   viewed from different angles. KEY AGENTS (by structural relationship): -
 *   farmed_and_laboratory_animals: Primary target (powerless/trapped) — bears
 *   the full bodily costs of permitted use - animal_use_industries: Primary
 *   beneficiary (institutional/constrained) — collects products, operating
 *   license, and label legitimacy; co-shapes the standards -
 *   animal_product_consumers: Secondary beneficiary (moderate/mobile) —
 *   receives goods and moral reassurance; cheapest exit in the story, least
 *   taken - welfare_regulatory_agencies: Administrator
 *   (institutional/constrained) — drafts, enforces, and incrementally revises
 *   the standards - abolitionist_advocates: Excluded critic
 *   (organized/identity_locked) — rejects the framework's premise; holds no
 *   seat in standard-setting - animal_welfare_science_community: Analytical
 *   observer (institutional/analytical) — supplies the evidence base;
 *   advisory leverage only
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.52).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Welfare-Constrained Animal Use Regime (Welfare Reading of the Animal-Status Kernel)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral philosophy/animal ethics/legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '67ea8cec-bcf9-48cd-8080-4dd294549c42').
narrative_ontology:cs_kernel_codification('67ea8cec-bcf9-48cd-8080-4dd294549c42', formalized).
narrative_ontology:cs_authority_grounding('67ea8cec-bcf9-48cd-8080-4dd294549c42', expertise).
narrative_ontology:cs_interpretation_layer_present('67ea8cec-bcf9-48cd-8080-4dd294549c42').
narrative_ontology:cs_reading_relation('67ea8cec-bcf9-48cd-8080-4dd294549c42', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('67ea8cec-bcf9-48cd-8080-4dd294549c42', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('67ea8cec-bcf9-48cd-8080-4dd294549c42', foundational, sentience_grounds_moral_considerability).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_considerability, holdable).
narrative_ontology:cs_axiom_grounding('67ea8cec-bcf9-48cd-8080-4dd294549c42', sentience_grounds_moral_considerability, deontological).
narrative_ontology:cs_axiom('67ea8cec-bcf9-48cd-8080-4dd294549c42', foundational, pain_minimized_use_is_permitted).
narrative_ontology:cs_axiom_status(pain_minimized_use_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('67ea8cec-bcf9-48cd-8080-4dd294549c42', pain_minimized_use_is_permitted, instrumental).
narrative_ontology:cs_axiom('67ea8cec-bcf9-48cd-8080-4dd294549c42', secondary, property_status_compatible_with_welfare_duties).
narrative_ontology:cs_axiom_status(property_status_compatible_with_welfare_duties, holdable).
narrative_ontology:cs_axiom_grounding('67ea8cec-bcf9-48cd-8080-4dd294549c42', property_status_compatible_with_welfare_duties, conventional).
narrative_ontology:cs_reference_frame('67ea8cec-bcf9-48cd-8080-4dd294549c42', welfare_constrained_property_status).
narrative_ontology:cs_drift_state('67ea8cec-bcf9-48cd-8080-4dd294549c42', contemporary_cognitive_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67ea8cec-bcf9-48cd-8080-4dd294549c42', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_and_laboratory_animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, sentience_moral_relevance_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, welfare_regulation_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raises, slaughters, or experiments on animals as the core of its business model. Pays compliance costs for welfare standards — facility retrofits, stunning equipment, audit fees — and receives in exchange a stable legal license to operate plus certification labels that sustain consumer demand. Shapes the standards themselves through lobbying and technical consultation. Leaving animal use would mean abandoning sunk capital, supply contracts, and brand identity built around animal products; some firms diversify, most do not.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_use_industries, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_use_industries, agenda_setter).

% Buys meat, dairy, eggs, and other animal products at prices kept low by intensive production, and receives moral reassurance through welfare labels that the animals involved did not suffer gratuitously. Any individual member can stop buying animal products at any time — plant-based substitutes exist and the exit carries little material penalty — yet aggregate demand keeps growing, suggesting the exit is easy but rarely taken.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Are bred, confined, handled, and killed inside the terms the standards permit. They receive whatever protection the current rules grant — anesthesia, space allowances, stunning — and bear everything the rules allow: short lives, dense housing, routine procedures without consent. They cannot refuse participation, cannot leave, cannot organize, and appear in the arrangement only as objects of measurement and management.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_and_laboratory_animals, payer,
    powerless, biographical, trapped, global).

% Draft, enact, and inspect against the treatment standards: setting space allowances, licensing slaughter operations, prosecuting violations. Their mandate and budget depend on the arrangement continuing; they are bound by statute, subject to industry lobbying and consumer sentiment, and revise standards incrementally as welfare science advances. They cannot abandon the framework without legislative action.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Campaign publicly against the entire framework, arguing that regulating use legitimizes it and that the animals' status as property is itself the wrong to be removed. They litigate, document conditions, run ballot initiatives, and pressure retailers, but hold no seat in the standard-setting bodies where industry and agencies negotiate the rules. Their opposition defines their public identity; abandoning it would dissolve their organizations' reason for existing.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% Studies animal cognition, stress physiology, and behavior; supplies the evidence base on which standards are calibrated; sits on advisory committees and publishes assessments of gaps between current rules and current knowledge. Neither collects the products nor bears the treatment; its leverage runs through credibility and advisory access.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_science_community, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides enforceable minimum-treatment standards for a society that has decided to keep using animals: stunning-before-slaughter rules, housing density limits, anesthesia requirements in research, transport duration caps — solved centrally instead of case-by-case. Certification labels additionally let consumers delegate moral diligence to third-party auditors.
% TRANSFER_FUNCTION: Moves the bodily costs of animal use — confinement, pain, premature death — onto the animals themselves; moves compliance costs onto producers; moves moral reassurance and product supply to consumers; moves inspection labor and regulatory mandate to public agencies.
% ABSENT_VOICES: The animals who bear the arrangement's costs have no seat and no voice of their own; their interests appear only as proxy measurements chosen by others. Abolitionist critics who reject the framework's premise (that use-with-welfare is acceptable) are outside the standard-setting negotiations. Slaughterhouse workers bearing psychological costs are only marginally seated through labor channels.
% DISAPPEARANCE_RATIONALE: If welfare regulation vanished overnight, animal use would continue at scale but without binding treatment floors; certification markets would collapse or turn fraudulent; consumer trust structures would rebuild around private schemes of uncertain rigor; litigation and advocacy would shift targets; the food-and-research compliance apparatus would reorganize around voluntary codes.
% FOUNDING_PROBLEM: Gratuitous cruelty in animal use — first wanton abuse by servants and carters addressed by the early anti-cruelty statutes, later the systematic suffering of intensive confinement and mechanized slaughter as production scaled beyond public visibility.
% FOUNDING_PROBLEM_CORROBORATION: Animal-welfare science documents persistent suffering inside fully compliant systems (regulatory-agency scientific opinions, peer-reviewed ethology); abolitionist philosophers attest the problem is live while disputing the remedy; recurring undercover investigations corroborate from outside official channels. None of these sources sits inside the industry beneficiary set.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.52 at interval end): the arrangement imposes real, morally weighted costs on animals — confinement, painful procedures, premature death — while welfare floors remove the worst excesses; the series shows early improvement as first-generation standards took effect, then plateau and slight reversal as absolute animal numbers grew faster than per-animal gains. Suppression (0.62) reflects the arrangement's holding force: animals have no exit of any kind, and the human side is held by licensed legitimacy — dissent is channeled into the very certification apparatus being criticized. Theater (0.34, rising) tracks the certification layer: stunning mandates and anesthesia rules do real work, but a growing share of activity is label maintenance whose welfare content is thin. Accessibility collapse is moderate (0.42): understanding the arrangement does not force acceptance of it — abolitionist and property alternatives remain live positions, and plant-based substitutes exist. Resistance (0.55) is bidirectional: abolitionists attack the framework's premise while industry lobbies against its tightening. All three series share one time grid (t=0..60, decade steps, roughly 1964–2024); the smooth trends summarize scandal-driven reform pulses (exposé, reform, relaxation, accumulation). fixing_cost is authored prohibitive: the seats able to change the arrangement (legislatures, agencies) face costs — industry lobbying, food-price effects, trade exposure — that exceed what they themselves bear of its defects.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently. From the animals' position the arrangement is near-total imposition with zero exit — no refusal, no departure, no coalition — computing toward the harshest experience available. From the industry seat the same arrangement reads as manageable compliance bundled with the license and label legitimacy that keep demand flowing — computing toward coordination it pays modestly for. The agency seat sees administrable, incrementally improvable standards; the abolitionist seat sees a legitimation machine manufacturing moral comfort; the consumer seat barely registers the arrangement at all given how easy its exit is. The engine derives these divergences from the structural data; nothing here reconciles them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive the derivation: animal_use_industries collect the arrangement's central flows (products, license, label premium) and sit near the beneficiary end, though sunk capital and brand identity pull them off the pure-beneficiary pole; animal_product_consumers benefit incidentally and hold the cheapest exit in the story, placing them nearest the beneficiary pole despite indirectly funding inspection through taxation. farmed_and_laboratory_animals are the declared victims: full targets, trapped exit, d at the maximum end. welfare_regulatory_agencies administer without collecting — no direct flow either way, near-symmetric. abolitionist_advocates hold no flow position; their relationship is oppositional, and the derivation places them mid-range rather than at either pole. vindicated_propositions (sentience doctrine, welfare-regulation legitimacy) collect no rents and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. Reading the arrangement as pure coordination (its self-presentation: humane regulation solving cruelty) erases the declared victim set — the animals who pay in full for everyone else's benefit. Reading it as pure extraction (the abolitionist framing: a comfort machine for exploitation) erases the documented suffering reduction the standards deliver. The tangled-rope structure holds both: genuine coordination function, asymmetric payment, active enforcement. On the genealogy interview: the founding problem (gratuitous cruelty) is live, and the disappearance verdict is world_rearranges, so no zombie flag fires — the arrangement has not outlived its problem. The watch item is the theater series: certification-layer drift is the classic path by which a tangled rope decays toward performance, and the rising theater ratio is the early indicator. Identity-lock notes: industry actors are institutionally fused with animal production ('we feed people'), making diversification rare despite its availability; abolitionist advocates are ideologically fused with their opposition, making compromise structurally unthinkable for them; consumers remain only weakly attached, which is why their exit is cheap and rarely taken — habit, not identity, holds them. Coalition check: the victim class cannot form coalitions directly (no organization capacity at scale); surrogate advocacy organizations exist but face the proxy problem — the interests they advance are inferred, never expressed by the bearers themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the welfare_reading instantiation of the animal_status_kernel; what structural facts would change if a sibling reading were adopted instead?',
    'No dataset resolves a reading choice; resolution is commitment-level. Track which reading comes to dominate doctrine and statute: if courts begin treating animals as rights-bearers, the abolitionist reading displaces this one; if welfare statutes are rolled back as overreach, the property reading returns.',
    'Under the abolitionist reading the victim set becomes all animals held as property and epsilon rises sharply, since property status itself is the injury; under the property reading animals drop out of the victim set entirely and epsilon collapses toward zero. This file''s moderate epsilon is indexed to the welfare reading alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in the victim-set boundary.').

omega_variable(
    happy_meat_demand_effect,
    'Do welfare reforms reduce total animal suffering, or do they expand and entrench use enough that net suffering rises (the new-welfarism critique)?',
    'Longitudinal comparison of per-animal welfare gains against total animals used, prices, and consumption volumes following major reforms and certification campaigns; natural experiments where comparable reforms were rejected.',
    'If demand expansion outweighs per-animal gains, the arrangement''s coordination function is partly illusory and its computed classification drifts toward pure extraction; if per-animal gains dominate, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(happy_meat_demand_effect, empirical, 'Whether welfare reform is net suffering-reducing or demand-expanding.').

omega_variable(
    welfare_proxy_validity,
    'Do the measurable proxies welfare science uses (stress hormones, stereotypies, lameness scores) track the morally relevant suffering this reading commits itself to taking seriously?',
    'Convergent validation across behavioral, physiological, and cognitive indicators; focused study of cases where the proxies and plausible experiential analogues diverge.',
    'Systematic underestimation would place the arrangement''s true extractiveness above the authored value; validated proxies would support the moderate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_proxy_validity, empirical, 'Validity of welfare metrics as measures of morally relevant suffering.').

omega_variable(
    suppression_structure_split,
    'Is the arrangement''s holding force maintained more by structural barriers (legal thinghood, no standing, physical confinement) or by internalized human attitudes (moral comfort supplied by humane labeling)?',
    'Compare jurisdictions differing in legal-standing provisions at similar public-attitude profiles; track reform demand after exposure campaigns that pierce label reassurance.',
    'If internalized comfort does most of the holding, removing legal barriers alone would not change outcomes and reform must target perception; if structural barriers dominate, statutory change is the operative lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structure_split, conceptual, 'Structural versus internalized share of the arrangement''s persistence.').

omega_variable(
    partial_inclusion_boundary,
    'Where does this reading''s own partial inclusion of animals stop — which species and capacities count as suffering-relevant (fish, cephalopods, insects, fetal states)?',
    'Comparative sentience science on nociception, consciousness markers, and behavioral complexity across taxa; legislative line-drawing episodes such as cephalopod inclusion in recent welfare rules.',
    'A wider boundary expands the victim set and raises effective extractiveness within this reading; a narrower one shrinks it. The reading''s epsilon is not stable until this boundary settles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partial_inclusion_boundary, empirical, 'Intra-reading contestation over the victim-set boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_reading_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(animal_welfare_reading_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(animal_welfare_reading_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(animal_welfare_reading_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(animal_welfare_reading_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(animal_welfare_reading_tr_t50, animal_status_kernel__welfare_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement(animal_welfare_reading_tr_t60, animal_status_kernel__welfare_reading, theater_ratio, 60, 0.34).

% Extraction over time
narrative_ontology:measurement(animal_welfare_reading_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement(animal_welfare_reading_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(animal_welfare_reading_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(animal_welfare_reading_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(animal_welfare_reading_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(animal_welfare_reading_be_t50, animal_status_kernel__welfare_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(animal_welfare_reading_be_t60, animal_status_kernel__welfare_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_reading_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(animal_welfare_reading_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(animal_welfare_reading_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(animal_welfare_reading_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(animal_welfare_reading_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(animal_welfare_reading_su_t50, animal_status_kernel__welfare_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(animal_welfare_reading_su_t60, animal_status_kernel__welfare_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'animal status' decomposes into three structurally distinct constraints sharing one kernel. The property reading places animals wholly outside the victim set (epsilon near zero by its lights); the welfare reading (this file) partially includes them via suffering-capacity (epsilon moderate); the abolitionist reading includes them fully as persons and locates the injury in property status itself (epsilon high). The upstream reading with the longest doctrinal lineage (property) historically anchored the other two as reactions; this file links both siblings so contamination and foreclosure analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
