% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Reading of Article 51 Self-Defense
 *   domain: international law / security studies / constitutional interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   article_51_self_defense — the expansive preventive reading: self-defense
 *   extends to preemptive and preventive force against non-state actors and
 *   emerging threats when the acting state demonstrates necessity. Under the
 *   epsilon-invariance principle the sibling readings
 *   (narrow_armed_attack_reading, unable_unwilling_doctrine_reading) are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this story, and the contest between readings is carried only in
 *   omega variables. The epsilon referent is the standing arrangement under
 *   contest — the operative practice of self-judged preventive force —
 *   assessed by this reading's own lights, never by the narrow reading it
 *   competes against. Structurally the arrangement coordinates a real
 *   residual function (a lawful channel for force when collective organs
 *   cannot act in time, and a shared legal vocabulary every state may
 *   formally invoke) while extracting asymmetrically: militarily capable
 *   states gain war-initiation latitude they administer themselves, defense
 *   industries collect sustained budgets, and the costs land on target-region
 *   populations, host-state sovereignty, and the bypassed multilateral veto
 *   authority. The claim/metric split is deliberate: claimed_type records my
 *   independent structural belief (tangled_rope); the metrics record
 *   descriptively true operation; the engine computes per-seat types from the
 *   structural data, and any divergence between claim and computation is the
 *   measurement the corpus exists to take. KEY AGENTS (by structural
 *   relationship): - militarily_capable_states: agenda-setter
 *   (institutional/arbitrage) — invokes the reading, self-judges necessity,
 *   conducts and narrates the operations - defense_industrial_base:
 *   beneficiary (organized/mobile) — collects procurement and sustainment
 *   revenue from continuously licensed operations -
 *   target_region_populations: primary target (powerless/trapped) — bear
 *   kinetic force with no seat in any interpreting forum and no exit from
 *   strike zones - sovereign_host_states: dual-positioned payer/beneficiary
 *   (moderate/constrained) — sovereignty compromised by operations on their
 *   territory while some tacitly gain from proxy removal of insurgents -
 *   multilateral_veto_authority: payer (institutional/trapped) —
 *   collective-security gatekeeping formally intact, practically bypassed -
 *   nonstate_armed_groups: payer (organized/constrained) — the ostensible
 *   targets; bear the strikes, hold no standing in the framework, cannot
 *   relocate beyond the doctrine's reach - small_nonaligned_states: excluded
 *   (powerless/trapped) — depend on the narrow reading for protection, object
 *   in the General Assembly, absent where doctrine forms -
 *   international_court_of_justice: observer (institutional/analytical) —
 *   adjudicates scope disputes with persuasive but weakly enforced authority
 *   - national_legislatures: excluded (moderate/constrained) — hold formal
 *   war powers in intervening states but receive self-judged necessity after
 *   operational commitment
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.74).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international law / security studies / constitutional interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, 'cb6759ce-eb63-498a-a212-252441f6ac6e').
narrative_ontology:cs_kernel_codification('cb6759ce-eb63-498a-a212-252441f6ac6e', fixed_text).
narrative_ontology:cs_authority_grounding('cb6759ce-eb63-498a-a212-252441f6ac6e', extraction).
narrative_ontology:cs_interpretation_layer_present('cb6759ce-eb63-498a-a212-252441f6ac6e').
narrative_ontology:cs_reading_relation('cb6759ce-eb63-498a-a212-252441f6ac6e', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('cb6759ce-eb63-498a-a212-252441f6ac6e', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('cb6759ce-eb63-498a-a212-252441f6ac6e', foundational, inherent_right_precedes_charter_text).
narrative_ontology:cs_axiom_status(inherent_right_precedes_charter_text, holdable).
narrative_ontology:cs_axiom_grounding('cb6759ce-eb63-498a-a212-252441f6ac6e', inherent_right_precedes_charter_text, deontological).
narrative_ontology:cs_axiom('cb6759ce-eb63-498a-a212-252441f6ac6e', foundational, self_judged_necessity_suffices).
narrative_ontology:cs_axiom_status(self_judged_necessity_suffices, holdable).
narrative_ontology:cs_axiom_grounding('cb6759ce-eb63-498a-a212-252441f6ac6e', self_judged_necessity_suffices, instrumental).
narrative_ontology:cs_axiom('cb6759ce-eb63-498a-a212-252441f6ac6e', secondary, threat_velocity_outpaces_collective_authorization).
narrative_ontology:cs_axiom_status(threat_velocity_outpaces_collective_authorization, holdable).
narrative_ontology:cs_axiom_grounding('cb6759ce-eb63-498a-a212-252441f6ac6e', threat_velocity_outpaces_collective_authorization, empirically_contingent).
narrative_ontology:cs_reference_frame('cb6759ce-eb63-498a-a212-252441f6ac6e', inherent_self_preservation_right).
narrative_ontology:cs_drift_state('cb6759ce-eb63-498a-a212-252441f6ac6e', post_iraq_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb6759ce-eb63-498a-a212-252441f6ac6e', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_base).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, sovereign_host_states).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, nonstate_armed_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, sovereign_host_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, self_judged_necessity_standard).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, elastic_imminence_custom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the reading, publish the necessity case through their own legal advisers, conduct the operations, and narrate outcomes. They select among legal framings, coalition partners, and forums as convenient, and bear little imposed cost when a necessity case fails. The gains of the arrangement — war-initiation latitude, procurement flows, precedent accumulation — accrue to their executives and institutions.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Collects procurement, sustainment, and munitions revenue from the continuous operations the reading licenses. It does not set the doctrine and does not administer it, but its revenue stream depends on the operational tempo the permissive trigger sustains, and it lobbies for capabilities that presuppose that tempo. Exit is real: capital and production lines can pivot to other markets and customers.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, global).

% Live where preventive strikes land. They are never the declared addressees of the necessity case, hold no seat in any court or council that interprets the doctrine, and cannot leave strike zones except into displacement. Their protection under the narrower reading — which required an actual or imminent attack before force — is precisely what this reading removes.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, regional).

% States on whose territory non-state actors operate and against which preventive force is used or threatened. Publicly they object to sovereignty violations; privately some tolerate or quietly welcome the removal of insurgents they cannot suppress themselves. Their consent is not required under this reading, which is the structural difference from the unable-unwilling sibling. Exit is limited: protest, asymmetric retaliation, or alignment shifts, all costly.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, sovereign_host_states, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, sovereign_host_states, beneficiary).

% The Security Council and the collective-security architecture it anchors. Its formal primacy over force authorization remains written into the Charter, but each self-judged invocation routes around it, and each routed-around case lowers the cost of the next. It cannot resign its position or exit the system it anchors; its remedy is procedural objection after the fact.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, trapped, global).

% Transnational armed networks that constitute the doctrine's named trigger. They bear the strikes directly, hold no legal standing in the framework that targets them, cannot surrender into any forum that would adjudicate their status, and cannot reliably relocate beyond the reach of the doctrine's long arm. Their dispersal into host populations is what pulls host-state sovereignty into the constraint's cost structure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, nonstate_armed_groups, payer,
    organized, biographical, constrained, global).

% States without projection capacity whose physical security depends on the narrow reading's protections. They vote against expansions in the General Assembly and are absent from the bilateral and coalition settings where the doctrine actually forms. They cannot exit the legal order whose interpretation is being shifted away from them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, small_nonaligned_states, excluded,
    powerless, generational, trapped, regional).

% Adjudicates disputes over the doctrine's scope when contentious jurisdiction exists, as in Nicaragua and Oil Platforms. Its holdings push against the expansive reading but are enforced only by the compliance of the same powerful states the reading benefits. It takes testimony, weighs state practice, and produces the analytical record the other seats argue over.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% Hold formal war-declaration and funding authority in many intervening states, but receive self-judged executive necessity after operational commitments are made or underway. Funding votes remain their lever; prior authorization is what the reading's self-judging structure bypasses.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, national_legislatures, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a lawful channel for immediate force response when collective-security organs cannot act in time, and supplies a shared legal vocabulary within which all states may formally claim legitimate defense; under this reading the trigger extends to preemptive and preventive force against non-state actors and emerging threats, with necessity assessed by the acting state.
% TRANSFER_FUNCTION: Moves war-initiation authority from collective bodies and target-state consent to the intervening state's executive; moves the risks of armed force onto target-region populations and host-state sovereignty; moves public funds toward defense procurement through sustained operational tempo.
% ABSENT_VOICES: Target-region populations hold no seat in any forum where the doctrine is interpreted or applied. Small non-aligned states that depend on the narrow reading speak in the General Assembly but are absent from the bilateral and coalition settings where doctrine actually forms. National legislatures formally hold war-declaration authority in many intervening states yet are presented with self-judged necessity after operational commitment.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, every operation currently justified as preventive self-defense would lose its legal frame: intervening states would have to route force decisions through attribution-and-imminence requirements or seek Security Council authorization, host-state consent would regain veto weight, and ongoing campaigns would pause or continue under open defiance of the jus ad bellum. Alliance law, domestic war-powers practice, and defense planning documents all reference the reading — the surrounding arrangements reorganize.
% FOUNDING_PROBLEM: The Article 51 kernel was built to reconcile state survival with collective security: a state facing armed attack must be able to respond lawfully before the Security Council can act. This reading's specific founding problem emerged when transnational terrorist networks and proliferating emerging threats no longer presented as attributable, imminent state attacks — the 2001-2003 problem of threats that fit neither the Charter's text nor its anticipated state-on-state grammar.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the 9/11 attacks themselves attest that transnational non-state violence was real and strained the narrow trigger. Against the reading's extensions: the Chilcot Inquiry found the necessity case for the 2003 invasion rested on flawed threat assessments; ICJ jurisprudence (Nicaragua 1986, Oil Platforms 2003) rejected necessity claims untethered to actual armed attack; the 2005 World Summit Outcome reaffirmed a narrow framing over explicit opposition. No body outside the invoking states attests that self-judged preventive necessity remains necessary today — the live-status attestation comes from the beneficiary set itself, which is itself signal.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 because the arrangement transfers war-initiation authority to the very seat that exercises it and concentrates costs on parties with no reciprocal lever: populations under strike, host sovereignties, and the collective-security organ. Suppression is authored at 0.62 as a RAW structural property — unscaled by power or scope — reflecting that persistence depends on marginalizing the Council-authorization route and resisting counter-doctrine rather than on participant preference; the enforcement burden stays modest because the beneficiaries hold the enforcement capacity. Theater_ratio 0.48: necessity demonstrations mix real evidentiary and alliance-management work with post-hoc rationalization — the 2005 peak in the series marks the WMD intelligence failure, where demonstration was largely retrospective. Accessibility_collapse 0.52: alternatives (Council authorization, narrow-reading litigation, domestic war-powers checks) survive but are degraded, not eliminated. Resistance 0.6: ICJ rulings, General Assembly majorities, scholarly consensus, and periodic allied refusal meet each extension. The measurement series run on ONE shared grid (2001/2005/2009/2013/2017/2021/2025) with every tracked metric authored at every point; end-state values equal the base_properties scalars. The arc is monotone-with-a-bump rather than cyclical: assertion (2001-2005), partial retrenchment rhetoric with drone-program normalization beneath it (2009-2013), then settled contested practice. suppression_requirement is tracked because this story specifically traces enforcement-machinery change — coalition construction and precedent-stacking rising to 2005, then plateauing — not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the intervening power's seat the reading presents as a minimal, near-vacuous constraint — a permission structure it administers alone — and its lived experience approaches rope: it built the channel, it judges the trigger, it bears little imposed cost. From the target-population and multilateral seats the same structure operates as enforced extraction: force arrives on self-judged necessity, the veto they nominally hold is bypassed, and exit does not exist. Sovereign host states straddle the gap: publicly payer, privately sometimes beneficiary. The engine derives these divergent per-seat classifications from the structural data (roles, power, exit options); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. militarily_capable_states sit nearest the beneficiary pole: declared beneficiary, and arbitrage-grade exit across legal framings, coalitions, and forums damps effective extraction toward subsidy. defense_industrial_base likewise collects without administering the doctrine. target_region_populations sit nearest the full-target pole: declared victims, powerless, trapped — absence of exit amplifies effective extraction. nonstate_armed_groups are targets with organized power but constrained mobility: relocation is possible, but the doctrine's declared reach follows them across borders. sovereign_host_states derive mid-to-high directionality from victim status, tempered by the declared secondary beneficiary role where tacit consent operates. multilateral_veto_authority derives high directionality from victim status; its retained formal primacy is described in its situation text rather than introduced as an override. Excluded seats (small_nonaligned_states, national_legislatures) carry exposure without voice — they inform the absent-voices answer, not the directionality arithmetic. No directionality overrides are authored: the derivation chain from declarations plus exit options already lands each seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The kernel's founding problem — lawful immediate response when the Council cannot act — remains live at the margin: genuinely imminent cross-border plots occur and the narrow trigger strains against them. But this reading's distinctive extension (preventive, self-judged) answered a problem whose flagship instances proved misdescribed (Iraq's absent weapons), and the arrangement now persists substantially on precedent and capability rather than on demonstrated necessity cases. Classification as tangled_rope prevents double mislabeling: a pure-snare verdict would erase the real coordination residue (rapid-response legality, the formally universal right every state retains on paper); a pure-rope verdict would erase the asymmetric extraction the same structure delivers. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: arrangements demonstrably depend on the reading, so no dead-mandate/zombie flag fires — the honest finding is a live-but-contested mandate carried by an increasingly extractive structure, which is the tangled-rope signature rather than mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel article_51_self_defense (the expansive_preventive_reading); which reading governs any particular use of force is settled by the acting state''s own institutions — what would the classification become under each sibling reading?',
    'An authoritative interpretation event (an ICJ ruling accepted by the principal practitioners, or charter-level clarification) or convergent state practice visibly abandoning one reading.',
    'Under narrow_armed_attack_reading, epsilon falls sharply and the victim set shrinks toward actual attackers; under unable_unwilling_doctrine_reading, extraction is intermediate and a host-state consent-like condition is restored. The disagreement is located in trigger breadth and in the locus of the necessity judgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: sibling readings instantiate materially different constraints with different epsilon and victim sets.').

omega_variable(
    necessity_predicate_falsifiability,
    'Is ''necessity demonstrated'' ever falsifiable ex ante when the demonstrating agent is the acting state itself?',
    'Comparative audit of invoked necessities against later-declassified assessments (Chilcot-pattern inquiries) across a corpus of invocations.',
    'If demonstration is systematically unfalsifiable, the coordination half of the tangled_rope reading collapses toward cover and the computed type shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_predicate_falsifiability, empirical, 'Whether the necessity predicate binds or merely decorates self-judged force decisions.').

omega_variable(
    formal_reciprocity_vs_exercise_capacity,
    'The doctrine is formally universal — any state may invoke it — but exercisable only by states with power-projection capacity: is the formal symmetry a genuine coordination feature or masking structure?',
    'Compare the deference granted to structurally equivalent invocations by strong versus weak states (e.g., Turkish cross-border operations versus comparable interventions by permanent Security Council members).',
    'If deference tracks capability rather than conduct, the effective beneficiary set reduces to the capable subset and the rope-side coordination claim weakens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_reciprocity_vs_exercise_capacity, empirical, 'Whether nominal universality of the self-defense right is load-bearing or decorative.').

omega_variable(
    normalization_vs_enforcement_decay,
    'Does the plateauing suppression_requirement reflect genuine customary acceptance of the reading, or decay of resistance capacity among would-be challengers?',
    'Observe whether rising powers adopt or reject the reading as they acquire intervention capability.',
    'If rising powers reject it, the plateau is enforcement decay and persistence is contingent on current hegemony; if they adopt it, normalization is real and the constraint hardens into custom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_vs_enforcement_decay, empirical, 'Distinguishing customary consolidation from hegemon-dependent tolerance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51_expansive_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(a51_expansive_tr_t2001, observed).
narrative_ontology:measurement(a51_expansive_tr_t2005, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement_basis(a51_expansive_tr_t2005, observed).
narrative_ontology:measurement(a51_expansive_tr_t2009, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2009, 0.42).
narrative_ontology:measurement_basis(a51_expansive_tr_t2009, observed).
narrative_ontology:measurement(a51_expansive_tr_t2013, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2013, 0.46).
narrative_ontology:measurement_basis(a51_expansive_tr_t2013, observed).
narrative_ontology:measurement(a51_expansive_tr_t2017, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2017, 0.5).
narrative_ontology:measurement_basis(a51_expansive_tr_t2017, observed).
narrative_ontology:measurement(a51_expansive_tr_t2021, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2021, 0.47).
narrative_ontology:measurement_basis(a51_expansive_tr_t2021, observed).
narrative_ontology:measurement(a51_expansive_tr_t2025, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(a51_expansive_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(a51_expansive_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(a51_expansive_be_t2001, observed).
narrative_ontology:measurement(a51_expansive_be_t2005, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement_basis(a51_expansive_be_t2005, observed).
narrative_ontology:measurement(a51_expansive_be_t2009, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2009, 0.63).
narrative_ontology:measurement_basis(a51_expansive_be_t2009, observed).
narrative_ontology:measurement(a51_expansive_be_t2013, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement_basis(a51_expansive_be_t2013, observed).
narrative_ontology:measurement(a51_expansive_be_t2017, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement_basis(a51_expansive_be_t2017, observed).
narrative_ontology:measurement(a51_expansive_be_t2021, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement_basis(a51_expansive_be_t2021, observed).
narrative_ontology:measurement(a51_expansive_be_t2025, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(a51_expansive_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(a51_expansive_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement_basis(a51_expansive_su_t2001, observed).
narrative_ontology:measurement(a51_expansive_su_t2005, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(a51_expansive_su_t2005, observed).
narrative_ontology:measurement(a51_expansive_su_t2009, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement_basis(a51_expansive_su_t2009, observed).
narrative_ontology:measurement(a51_expansive_su_t2013, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement_basis(a51_expansive_su_t2013, observed).
narrative_ontology:measurement(a51_expansive_su_t2017, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement_basis(a51_expansive_su_t2017, observed).
narrative_ontology:measurement(a51_expansive_su_t2021, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement_basis(a51_expansive_su_t2021, observed).
narrative_ontology:measurement(a51_expansive_su_t2025, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(a51_expansive_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 51 self-defense' decomposes, per the epsilon-invariance principle, into three structurally distinct readings: this expansive preventive reading (trigger extends to preemptive/preventive force; necessity self-judged; epsilon approximately 0.74 with victims among target populations, host sovereignties, and the multilateral veto authority), the narrow armed-attack reading (trigger confined to actual/imminent attributable attack; epsilon far lower; victims essentially limited to actual aggressors), and the unable-or-unwilling hybrid (trigger conditioned on host-state failure; intermediate epsilon restoring a consent-like condition). The readings differ on trigger breadth and on where the necessity judgment sits — changing the observable (whose necessity counts) changes epsilon, which is why they are separate files linked here rather than one story with a measurement parameter. The upstream narrow reading is the baseline the expansive reading's advocates cite themselves as extending; the unable-unwilling reading functions as the negotiated midpoint whose legitimacy conditions this reading's practice continuously shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
