% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)
 *   domain: legal philosophy/constitutional law/rights theory
 *
 * SUMMARY:
 *   Every legal order must fix a class of rights-and-duty bearers; this story
 *   authors one reading of where that line sits. The restrictive
 *   anthropocentric reading places personhood at live birth and bounds it by
 *   species: born humans with cognitive capacity are persons; prenatal life,
 *   non-human animals, ecosystems, and artificial systems are not. The
 *   standing arrangement under contest is the boundary as currently operated
 *   — courts denying personhood petitions, legislatures writing definitional
 *   statutes that stop short of expansion. Because epsilon is reading-indexed
 *   over that fixed referent, this file authors epsilon low (0.20): by this
 *   reading's own lights the boundary protects actual rightsholders and
 *   burdens no one with standing to complain. The sibling files —
 *   developmental_potentiality_reading and functional_capacity_reading —
 *   author sharply higher epsilon over the SAME referent, because each counts
 *   different cost-bearers inside the line. The kernel contest is therefore
 *   recorded here as structure plus omegas, never as hedged epsilon: one
 *   reading, one epsilon, one victim set. The claim/metric pairing is
 *   deliberate and untuned: claimed_type tangled_rope reflects a genuine
 *   coordination function (a determinate addressee class) joined to an
 *   asymmetric distribution (every exclusion maps onto an interest that
 *   profits from it), while the metrics describe modest extraction, a
 *   hardening enforcement ratchet, low theater, and very high resistance. KEY
 *   AGENTS (by structural relationship): - born_human_persons: paradigm
 *   rightsholders (organized/identity_locked) — the class the boundary
 *   defines and protects - pregnant_persons: autonomy beneficiaries
 *   (moderate/constrained) - resource_extraction_industries:
 *   externality-shielded beneficiaries (institutional/arbitrage) -
 *   agricultural_animal_enterprises: property-status beneficiaries
 *   (institutional/arbitrage) - ai_developers_and_deployers:
 *   instrument-status beneficiaries (institutional/arbitrage) -
 *   public_interest_standing_litigants: procedural payers
 *   (organized/constrained) — bear the costs of every denied expansion -
 *   judiciary: agenda_setter (institutional/constrained) — maintains the line
 *   opinion by opinion - legislative_bodies: agenda_setter
 *   (institutional/biographical) — writes the definitional statutes -
 *   legal_philosophy_scholars: analytical observers -
 *   boundary_excluded_entities: structurally silent non-parties (recorded,
 *   not adjudicated)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.55).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal philosophy/constitutional law/rights theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '080cf3a3-1fb9-4f5d-91a7-7091831866ef').
narrative_ontology:cs_kernel_codification('080cf3a3-1fb9-4f5d-91a7-7091831866ef', formalized).
narrative_ontology:cs_authority_grounding('080cf3a3-1fb9-4f5d-91a7-7091831866ef', lineage).
narrative_ontology:cs_interpretation_layer_present('080cf3a3-1fb9-4f5d-91a7-7091831866ef').
narrative_ontology:cs_reading_relation('080cf3a3-1fb9-4f5d-91a7-7091831866ef', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('080cf3a3-1fb9-4f5d-91a7-7091831866ef', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('080cf3a3-1fb9-4f5d-91a7-7091831866ef', foundational, live_birth_triggers_personhood).
narrative_ontology:cs_axiom_status(live_birth_triggers_personhood, holdable).
narrative_ontology:cs_axiom_grounding('080cf3a3-1fb9-4f5d-91a7-7091831866ef', live_birth_triggers_personhood, conventional).
narrative_ontology:cs_axiom('080cf3a3-1fb9-4f5d-91a7-7091831866ef', foundational, human_species_membership_bounds_personhood).
narrative_ontology:cs_axiom_status(human_species_membership_bounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('080cf3a3-1fb9-4f5d-91a7-7091831866ef', human_species_membership_bounds_personhood, deontological).
narrative_ontology:cs_axiom('080cf3a3-1fb9-4f5d-91a7-7091831866ef', secondary, racial_bounds_on_personhood).
narrative_ontology:cs_axiom_status(racial_bounds_on_personhood, overridden).
narrative_ontology:cs_axiom_grounding('080cf3a3-1fb9-4f5d-91a7-7091831866ef', racial_bounds_on_personhood, conventional).
narrative_ontology:cs_reference_frame('080cf3a3-1fb9-4f5d-91a7-7091831866ef', birth_species_personhood_line).
narrative_ontology:cs_drift_state('080cf3a3-1fb9-4f5d-91a7-7091831866ef', contemporary_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('080cf3a3-1fb9-4f5d-91a7-7091831866ef', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_human_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, agricultural_animal_enterprises).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_developers_and_deployers).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, public_interest_standing_litigants).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_positivist_personhood_convention).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, injury_in_fact_standing_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the full bundle of legal rights and duties: they can own, contract, sue, vote, and invoke constitutional protection. Every institution that allocates rights addresses them by name. They fund the court system that adjudicates who else may join this class, and they disagree among themselves about where the outer edge of the class should sit. Leaving the class is not an option available to anyone; membership is constitutive of their legal identity.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_human_persons, beneficiary,
    organized, biographical, identity_locked, global).

% Make decisions about pregnancy under the shield of prenatal exclusion from legal personhood: no state claim counters theirs in the decision, and no fetal estate, trust, or cause of action competes with their bodily liberty. Their legal exposure changes entirely in jurisdictions or moments where a rival placement of the line prevails, and they cannot opt out of the jurisdiction that governs their pregnancies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Operate mines, wells, timber concessions, and emissions streams whose affected ecosystems have no procedural voice of their own. Challenges to their activity arrive only through government agencies or human plaintiffs meeting injury requirements; the operating envelope is set by regulation they lobby over, not by rightsholders they must answer to. They can relocate operations across jurisdictions when local rules tighten.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, resource_extraction_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Raise, transport, and process animals that the law classes as property. Welfare statutes set floors; no animal appears as a plaintiff, and no third party may sue solely on an animal's behalf. Most enforcement is driven by investigation and documentation, at political and compliance costs the enterprises contest. Production can shift to permissive jurisdictions when standards tighten.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, agricultural_animal_enterprises, beneficiary,
    institutional, generational, arbitrage, continental).

% Build and deploy systems that remain products and instruments however capable they become. No duty is owed to the systems themselves, no claim can be made in their name, and operators carry ordinary product liability rather than anything resembling obligations toward a dependent rightsholder. Development can move to favorable regulatory environments at low cost.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_developers_and_deployers, beneficiary,
    institutional, generational, arbitrage, global).

% Bring suits seeking procedural footholds for rivers, animals, or prenatal life: next-friend petitions, guardianship applications, citizen-suit provisions pushed to their limits. They lose most standing motions, absorb the litigation costs, and their repeated losses are the day-to-day work by which the line is kept where it is. Alternative forums are few: legislative routes exist but are slower and frequently preempted once a locality innovates.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, public_interest_standing_litigants, payer,
    organized, generational, constrained, national).

% Adjudicate every petition that asks the line to move: habeas filings for elephants, personhood assertions for embryos, injunction requests brought in a river's name. Each denial restates the boundary and binds later panels. Precedent leaves little room to distinguish around a petition, and each published denial becomes the next movant's target. Individual judges rotate; the office persists.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Write the definitional statutes: vital-statistics acts, animal welfare codes, corporate personality provisions, and unborn-victims-of-crime laws drafted to stop short of personhood. Electoral cycles make the line a recurring campaign subject; expansions pass in some jurisdictions and are repealed or preempted in others. A body can rewrite its own definitions but inherits the constitutional constraints above it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legislative_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% Map the boundary's structure from outside the fight: comparative surveys of where different systems draw it, histories of its previous migrations, and arguments for and against each rival placement. They hold no votes and file no petitions; their influence runs through opinions, briefs, and the education of future judges.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_philosophy_scholars, observer,
    analytical, civilizational, analytical, global).

% Entities on the far side of the line — prenatal life, sentient animals, ecosystems, and artificial systems — have no procedural voice of their own. Whatever claims might be made in their name arrive only through human proxies, whose petitions are dismissed for want of a qualifying rightsholder. They are recorded here to mark the structural silence the line produces; this entry does not resolve whether they hold the relevant kind of status.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, boundary_excluded_entities, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, boundary_excluded_entities).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, resource_extraction_industries).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes, once and centrally, the class of entities that can hold rights, owe duties, own property, contract, sue, and be sued — so that every other legal institution (contract, tort, crime, standing, succession) has a stable addressee and no case turns on re-deriving basic legal capacity from scratch.
% TRANSFER_FUNCTION: Moves full legal recognition and protection to born humans; moves the costs of activities affecting entities outside the line onto those entities and onto any advocates who attempt procedural representation of them; and removes prenatal counterclaims from pregnancy decision-making, leaving that domain to the pregnant person.
% ABSENT_VOICES: The entities on the far side of the line have no voice of their own; their would-be proxies appear in court only to lose standing motions. Within the human conversation, expansion movements are heard politically but rarely win doctrinal footholds, and the entities themselves are absent by design — which is precisely what the rival readings dispute.
% DISAPPEARANCE_RATIONALE: If the line vanished overnight, every doctrine that presupposes a person/thing distinction — ownership, contract, criminal responsibility, standing, succession — loses its addressee class simultaneously; courts would have to rebuild a boundary from first principles within days, and whichever rebuilt line emerged would be one of the three readings already in contention.
% FOUNDING_PROBLEM: Roman-law and early-modern jurisprudence needed to fix which entities are subjects rather than objects of legal relations — who can be a plaintiff, an owner, an heir, a defendant — and settled on the person/thing distinction that modern codes inherited.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and jurists across civil-law and common-law traditions attest that the person/thing distinction predates and grounds all three contemporary readings; their scholarship corroborates the founding problem's reality and persistence while taking no side on where the line belongs. No attestation comes from inside any benefiting party's brief.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.20 because the reading endorses the arrangement it evaluates: the boundary protects recognized rightsholders at low cost to them, and the residual extraction the reading itself concedes is the widening proxy-representation gap as economic activity scales. Suppression is 0.55 as a raw structural property — unscaled by power or scope, per the framework's division of labor; only extractiveness is scaled downstream. It reflects real enforcement machinery: standing doctrine, preemption of local innovations, summary dismissal practices. Theater is 0.15: maintenance is overwhelmingly functional adjudication, with a thin layer of symbolic statute-making. Accessibility collapse is 0.30 — deliberately low, because the kernel contest itself proves alternatives have not collapsed: rival placements remain live and re-litigated every session. Resistance is 0.72: abortion litigation, animal habeas campaigns, rights-of-nature ordinances, and AI personhood proposals constitute sustained, organized pressure. The measurement series run on one shared grid (t=0..50, six points per metric) so no metric borrows another's end-state; suppression_requirement is authored because this story specifically tracks enforcement-capacity change — the ratchet by which standing doctrine tightened, local rights-of-nature ordinances were preempted, and petitions came to be dismissed faster as contest volume grew.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat the boundary is settled law administered case by case; from the commercial beneficiaries' seats it is background freedom they rarely think about; from the standing litigants' seat it is a door that closes on filing day; from a sibling reading's seat the same line is a mass wrong. The engine computes these per-seat classifications from power, exit, and role — the divergence between the payer seat's computed type and the beneficiary seats' is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Born human persons derive near-full beneficiary directionality: declared beneficiaries whose exit is identity_locked — legal personhood is constitutive of who they are, not a position they chose. Pregnant persons likewise. The three commercial seats derive the lowest d of all: beneficiaries with arbitrage-grade exit who collect the externality surplus the line renders unlitigable. Public-interest standing litigants derive near-full target directionality: declared payers whose exit is constrained, since no alternative forum opens once standing is denied. The judiciary sits mid-range as agenda_setter — it collects authority and workload, not rents. Global spatial scope amplifies effective extraction modestly upward from the low base through the verification-difficulty modifier; the result remains low in absolute terms, consistent with the reading's own assessment of the arrangement it defends.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fixing a determinate addressee class for law — is live and corroborated from outside every benefiting party, so mandatrophy is not resolved and no sunset applies. The classification guards both mislabelings: calling the boundary a snare would erase the real coordination function (no legal order operates without a person/thing line); calling it a pure rope would erase the asymmetry (each exclusion is defended by interests that profit from it, and the enforcement ratchet tightens as contest grows). Tangled rope holds both truths at once. If the founding problem ever died — if law dispensed with fixed addressee classes — the arrangement would decay toward inertial maintenance; nothing in the current record suggests that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story evaluates the personhood boundary through the restrictive anthropocentric reading alone; how would the classification change under each sibling reading of the same kernel?',
    'Compile the two sibling stories and compare per-seat classifications over the identical referent; the victim-set deltas (prenatal life added by the developmental reading; sentient non-humans added and marginal humans subtracted by the functional reading) locate the disagreement structurally.',
    'Under the developmental reading epsilon over the same arrangement rises sharply and prenatal life enters the victim set; under the functional reading the species bound itself becomes the extraction site. This file''s low reading-indexed epsilon is valid only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'One reading of a three-reading kernel; epsilon is indexical to the reading, not the topic.').

omega_variable(
    marginal_human_capacity_scope,
    'Does the cognitive-capacity clause exclude any born humans — permanently unconscious or anencephalic persons — or is capacity read so minimally that every born human qualifies?',
    'Survey how jurisdictions actually treat birth registration, guardianship, and end-of-life law for cognitively devastated born humans: if any are denied personhood-relevant protection, the clause bites as a test rather than functioning as rationale.',
    'If the clause bites, a victim set appears inside this reading''s own line and its epsilon rises on its own terms; if it does not bite, the operative line is birth plus species alone and the current authoring stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_human_capacity_scope, empirical, 'Whether the capacity clause is a test that excludes marginal born humans or background rationale covering all of them.').

omega_variable(
    excluded_entity_status_contest,
    'Does the cost-bearing of entities outside the line — prenatal life, sentient animals, ecosystems, artificial systems — constitute victimization in the rights-relevant sense, or mere absence of status?',
    'Not resolvable inside this reading: the question IS the kernel contest. Resolution arrives only by adopting a sibling reading or by articulating a criterion of relevant status that all three readings accept and applying it.',
    'If a shared criterion emerges that the excluded entities meet, this file''s victim set is incomplete and its epsilon understated even by its own lights; if they do not meet it, the current authoring stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excluded_entity_status_contest, conceptual, 'The location of the kernel disagreement: whether structural cost-bearing outside the line is wrong-making.').

omega_variable(
    proxy_representation_adequacy,
    'Is human-proxy representation of excluded-entity interests adequate, or does the standing bar produce systematic underrepresentation that this reading must answer as an institutional-design matter?',
    'Compare regulatory outcomes for represented versus unrepresented externalities — agency-enforced limits versus litigated ones — and measure outcome divergence where proxies gained footholds (citizen-suit provisions, guardian statutes in a few jurisdictions).',
    'If proxies systematically underprotect, the reading owes an institutional answer and its internal epsilon rises; if agency representation tracks litigated outcomes, the policy-not-rights response holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_adequacy, empirical, 'Whether the standing bar leaves excluded-entity interests systematically underrepresented even on this reading''s own terms.').

omega_variable(
    enforcement_ratchet_projection,
    'Will the enforcement ratchet keep tightening (an escalating contest cycle) or plateau as expansion movements exhaust legislative and litigative routes?',
    'Track standing-denial rates, preemption enactments, and petition volumes past the interval endpoint; the series after t=50 would be projected, not observed.',
    'Continued tightening pushes the arrangement toward harder enforcement forms and raises the payer seat''s effective burden; plateau suggests the tangled form stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_ratchet_projection, empirical, 'Trajectory of the enforcement ratchet beyond the observed interval.').

omega_variable(
    identity_coordination_coupling_risk,
    'Does the identity_coordination framing of the boundary risk excusing coupling that concentrates burdens on the least powerful parties at the largest scope?',
    'Run the power-by-scope coupling check on the compiled story: the excluded parties are the least powerful seats at global scope, absorbing externalities they cannot litigate, while the offset for identity coordination tolerates complex boundary maintenance.',
    'If the coupling flags, the complexity offset must not launder the asymmetry: excess burden above the coordination floor should be attributed to the exclusion structure, not to legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_coupling_risk, conceptual, 'Guard against identity-framing serving as cover for asymmetric burden concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t0, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t10, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t20, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t30, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t40, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(lpb_restrictive_anthro_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t0, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t10, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t20, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t30, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t40, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(lpb_restrictive_anthro_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t0, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t10, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t20, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t30, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t40, observed).
narrative_ontology:measurement(lpb_restrictive_anthro_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(lpb_restrictive_anthro_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'who counts as a person?' decomposes, per the epsilon-invariance principle, into three structurally distinct boundary rules: conception-triggered and species-wide (developmental_potentiality_reading), capacity-triggered and species-neutral (functional_capacity_reading), and birth-triggered and species-bounded (this file). Each instantiates a different victim set and a different reading-indexed epsilon over the same standing arrangement; measuring one with another's observable changes epsilon, which is the signature of distinct constraints sharing a kernel rather than one constraint viewed from angles. Unlike the BGS family, the ordering here is a lateral contest rather than an evidentiary chain: each reading cites the others' perceived absurdities as support, and no member is upstream in confidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
