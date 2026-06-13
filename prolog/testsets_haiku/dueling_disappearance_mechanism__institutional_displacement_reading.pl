% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling as Dispute-Resolution Mechanism (Institutional Displacement Reading)
 *   domain: legal/cultural/historical
 *
 * SUMMARY:
 *   This reading frames dueling's historical disappearance as a case of
 *   institutional substitution: courts, banking law, and libel law matured
 *   and became accessible, offering faster, more reliable, more enforceable
 *   resolution of the disputes that dueling previously handled. Dueling
 *   persists not because it vanishes from human imagination or is culturally
 *   unthinkable, but because it becomes unnecessary—participants choose
 *   institutional alternatives when available. The constraint is classified
 *   as rope (voluntary coordination on a dispute-resolution protocol)
 *   throughout the interval, not as a snare or piton. Participants are
 *   beneficiaries (they voluntarily use dueling when it works), not victims
 *   (there is no enforced entrapment). The mechanism itself is not extracted
 *   from; rather, it becomes disfavored because competing mechanisms are more
 *   efficient.
 *
 * KEY AGENTS:
 *   - honor_culture_gentry: Participants in dueling; perceived benefit from rapid honor-dispute resolution
 *   - merchant_class_requiring_credit: Shift from honor to institutional credit as commerce scales
 *   - state_court_system: Expanding jurisdiction over disputes previously outside formal law
 *   - banking_and_commercial_law: Institutionalized credit, contracts, enforcement without personal honor
 *   - libel_law_framework: Legal remedy for reputation damage, displacing violent response
 *   - legal_profession: Beneficiaries of increased dispute volume routed through courts
 *   - state_authorities_enforcing_prohibition: Formal prohibition, but selective and conditional on institutional alternatives maturity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.22).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling as Dispute-Resolution Mechanism (Institutional Displacement Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "legal/cultural/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '4d20356a-2dfe-491d-afef-704b90d81350').
narrative_ontology:cs_kernel_codification('4d20356a-2dfe-491d-afef-704b90d81350', distributed).
narrative_ontology:cs_authority_grounding('4d20356a-2dfe-491d-afef-704b90d81350', practice).
narrative_ontology:cs_reading_relation('4d20356a-2dfe-491d-afef-704b90d81350', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d20356a-2dfe-491d-afef-704b90d81350', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('4d20356a-2dfe-491d-afef-704b90d81350', foundational, institutional_substitution_mechanism_primary).
narrative_ontology:cs_axiom_status(institutional_substitution_mechanism_primary, holdable).
narrative_ontology:cs_axiom_grounding('4d20356a-2dfe-491d-afef-704b90d81350', institutional_substitution_mechanism_primary, empirically_contingent).
narrative_ontology:cs_axiom('4d20356a-2dfe-491d-afef-704b90d81350', secondary, dueling_persists_in_institutional_gaps).
narrative_ontology:cs_axiom_status(dueling_persists_in_institutional_gaps, holdable).
narrative_ontology:cs_axiom_grounding('4d20356a-2dfe-491d-afef-704b90d81350', dueling_persists_in_institutional_gaps, empirically_contingent).
narrative_ontology:cs_reference_frame('4d20356a-2dfe-491d-afef-704b90d81350', honor_culture_dispute_resolution_system).
narrative_ontology:cs_drift_state('4d20356a-2dfe-491d-afef-704b90d81350', institutional_maturation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d20356a-2dfe-491d-afef-704b90d81350', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_participants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, merchant_class_requiring_credit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_gentry).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, legal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dueling provided direct, rapid dispute resolution for matters of honor and social standing that courts treated as trivial or refused to hear (insults, slights, betting disputes). Participants chose dueling because it resolved status claims faster than legal proceedings and produced an outcome courts and community recognized as binding.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_gentry, beneficiary,
    powerful, biographical, mobile, regional).

% Banking and commercial law provided written documentation, enforceable contracts, and credit instruments that made large-scale commerce possible. Merchants needed these tools more than honor-based dispute resolution; they chose courts and banking law when available because they handled the disputes that actually threatened their livelihoods and commercial networks.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, merchant_class_requiring_credit, beneficiary,
    organized, biographical, constrained, regional).

% Expanded jurisdiction, professionalized procedure, and enforcement machinery over the interval. Began handling disputes that dueling previously resolved: slander, debt, property boundaries, contractual breach. Did not ban dueling outright; instead made formal alternatives so reliable and accessible that dueling became an increasingly rare choice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_court_system, agenda_setter,
    institutional, generational, analytical, national).

% Developed written instruments, note-holding, and credit documentation that made large transactions possible without requiring the parties to know and trust each other personally. Displaced dueling as the mechanism for enforcing obligations because it made the obligations enforceable through courts rather than through personal honor.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_and_commercial_law, agenda_setter,
    institutional, generational, analytical, regional).

% Provided a legal remedy for false statements damaging reputation—precisely the category of dispute that dueling historically addressed. As libel law matured and became reliably available, it offered an alternative to violence for restoring reputation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_framework, agenda_setter,
    institutional, generational, analytical, national).

% Grew in size and social authority as courts expanded. Benefited from increased dispute volume routed through legal channels rather than dueling. Their interest lay in making legal dispute resolution so attractive and reliable that other mechanisms atrophied.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_profession, beneficiary,
    organized, generational, mobile, regional).

% Enacted formal prohibitions on dueling (criminal penalties for participation) and enforced them selectively. However, the reading's structural claim is that prohibition alone did not drive the mechanism's disappearance—institutional substitution did. Enforcement was necessary but insufficient; dueling persisted longest in regions where institutional alternatives were absent or unreliable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_authorities_enforcing_prohibition, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling coordinated rapid dispute resolution for honor-based claims (insults, slights, social standing questions) in a context where formal courts did not handle them. It also coordinated enforcement of personal honor and credibility without requiring written documentation or institutional mediation. The mechanism solved a real coordination problem: how do individuals establish and defend claims to status when official institutions are silent?
% TRANSFER_FUNCTION: Dueling transferred risk and potential death from the person making the claim (the offended party) to both parties equally, creating mutual incentive to resolve disputes through negotiation rather than proceed to the field. This created a pressure-valve coordination: many dueling insults never reached the field because both parties recognized the mutual cost and negotiated settlement. What moved was willingness to accept resolution as binding, created through shared risk exposure.
% ABSENT_VOICES: Those harmed by dueling (families of the dead, survivors with permanent injury, widows without support) had no seat at the table of honor-culture dispute resolution. The mechanism required no consent from those it affected. Sociologists and historians arguing dueling was barbaric and inefficient also lacked standing in the institutional system that was being displaced—their critique came from outside both the honor framework and the emerging institutional framework, making them observers only.
% DISAPPEARANCE_RATIONALE: If institutional substitutes (courts, banking law, libel law) had not matured and become accessible, dueling would have persisted as the default mechanism for honor disputes and credit enforcement. The dispute-resolution landscape would have remained fragmented: dueling for honor, courts for property, banking law for credit. When all three functions consolidated into institutional channels, dueling had nowhere to operate except in niches where institutions failed (frontier regions, isolated communities, military hierarchies).
% FOUNDING_PROBLEM: How do individuals establish and defend claims to social status, respond to insults, and enforce credit obligations in the absence of centralized institutional mediation?
% FOUNDING_PROBLEM_CORROBORATION: The institutional displacement argument is corroborated by legal historians (Wyatt-Brown, Freeman, McAleer) showing that dueling rates declined sharply as courts expanded jurisdiction and libel law matured. It is corroborated by economic historians showing that credit instruments and commercial law enabled long-distance transactions without personal honor as enforcement mechanism. It is corroborated by regional variation: dueling persisted longest where courts were distant, unreliable, or slow—confirming that institutional substitutes, not cultural values alone, drove the displacement. The honor-culture advocates (those who actually practiced dueling) do not today attest that the founding problem is live, because the institutional alternatives have proven effective.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because dueling is not extractive in structure—both parties accept the same risk; participation is voluntary; no concentrated beneficiary exploits the mechanism. Suppression is also low in the early interval (0.05 in 1700) because dueling is culturally legitimate and widely accepted. It rises to 0.22 by 1850 as state prohibition hardens and institutional alternatives mature—suppression is needed to enforce the formal ban. Theater ratio stays very low (0.08 max) because the mechanism serves its coordination function (dispute resolution) throughout—even when declining in frequency, it works when invoked. The measurement series show suppression rising sharply after 1800 (when prohibition becomes legally serious) while extractiveness stays flat (institutional substitution, not coercion, is the driver). This flat extractiveness despite rising suppression is the signature of institutional displacement: the constraint is being suppressed not because it became extractive, but because competing mechanisms are now available and preferred.
 *
 * PERSPECTIVAL GAP:
 *   The honor-culture gentry seat and the institutional agenda-setter seats experience the same constraint very differently. From the gentry perspective (1700-1750), dueling is the natural, legitimate, efficient dispute-resolution mechanism; institutional courts are slow, expensive, and refuse to hear honor cases. From the state court system perspective (1800-1900), dueling is a barbaric relic being phased out by superior institutional mechanisms; the constraint is becoming obsolete. The engine's per-seat computation should produce: gentry seats (early interval) computing dueling as rope/coordination (beneficiary position, choosing it voluntarily); later gentry seats (post-1850) computing it as piton (persisting by inertia in institutional gaps, but no longer genuinely functional); institutional agenda-setter seats computing it consistently as an obstacle to be suppressed throughout. The gap reflects real structural divergence in how the mechanism functions from different positions, not disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The honor-culture gentry are structural beneficiaries early in the interval—dueling provides the dispute resolution they need. By the late interval, they are more neutral: institutional alternatives are available and superior, but dueling remains an option in institutional gaps (identity_locked exit: honor-code commitment keeps some actors engaged even when alternatives exist). The merchant class begins as beneficiaries of dueling (honor-based credit enforcement) and transitions to beneficiaries of institutional credit mechanisms (banking law is more reliable). The legal profession gains as disputes route through courts. State court and banking systems are the primary agenda-setters, not beneficiaries in the traditional sense—they provide the institutional alternatives without directly collecting from dueling's decline. Suppression (via prohibition) increases but extractiveness does not, because the constraint is being outcompeted by superior mechanisms, not exploited.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by treating dueling as a mechanism whose founding problem genuinely died (institutional dispute-resolution alternatives solved it completely). The constraint persists post-death not because anyone benefits from its operation (mandatrophy signature: administration costs exceed collected benefits), but because institutional gaps exist and some actors remain identity-locked to honor codes. The theater ratio is stable and low (0.08), meaning the constraint's function (rapid dispute resolution) remains real even as the mechanism is suppressed—this distinguishes it from a piton (theater ratio rising as function atrophies but performance continues). The reading's classification as rope throughout reflects this: dueling is legitimate coordination on a dispute-resolution protocol, chosen voluntarily by participants who have better options but choose it anyway in specific contexts (frontier, military, isolated honor-culture communities). It is not a snare because there are no victims, and it is not a piton because function persists even as prevalence declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_substitution_vs_cultural_displacement,
    'Did dueling disappear primarily because institutional mechanisms outcompeted it in efficiency (institutional_displacement_reading) or because honor-culture axioms were replaced by dignity-culture axioms making dueling morally unthinkable (contraction_reading)?',
    'Regional variation analysis: if dueling persisted longest in regions where courts were distant/unreliable and declined fastest where institutional alternatives matured, institutional substitution is the driver. If dueling declined uniformly across regions regardless of institutional maturity, cultural displacement is the driver. Diary and correspondence analysis: if gentry continued to view dueling as legitimate after institutional alternatives were available, cultural displacement has not occurred.',
    'If institutional substitution dominates, the constraint type is rope throughout (voluntary coordination on competing protocol). If cultural displacement dominates, the constraint becomes piton late in interval (performing legitimacy the culture no longer endorses). If both operate, the reading requires an omega documenting the decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_substitution_vs_cultural_displacement, empirical, 'Mechanism of dueling''s decline: institutional outcompetition vs. cultural axiom replacement.').

omega_variable(
    institutional_gap_persistence,
    'Do modern institutional gaps (frontier regions, military subcultures, organized crime, prison hierarchies) continue to use dueling-equivalent mechanisms (ritualized violence, honor-based settlement) when formal institutions are absent or unreliable?',
    'Ethnographic and historical study of how disputes are resolved in institutional gaps: prison violence hierarchies, street gangs, military informal justice, frontier communities. Do the mechanisms structurally resemble dueling (bilateral, high-stakes, honor-based, community-witnessed, outcome-binding)?',
    'If dueling-equivalents persist in modern institutional gaps, the reading is supported—dueling did not become unthinkable, just unnecessary where institutions work. If dueling-equivalents are absent even in institutional gaps, cultural displacement is implicated—the axioms genuinely changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gap_persistence, empirical, 'Whether dueling-equivalent mechanisms persist in modern institutional gaps.').

omega_variable(
    founding_problem_death_vs_zombie_persistence,
    'Did the founding problem (defending honor and status claims, enforcing credit obligations through personal credibility) genuinely disappear, or does it persist in modified form even after institutional solutions matured?',
    'Examine whether reputation, social standing, and personal credibility continue to matter in institutional contexts: does a person with damaged reputation have difficulty accessing institutional remedies? Does a person with high social standing receive preferential treatment from courts and banks? If reputation still matters, the founding problem is not fully dead, and institutional substitution is partial.',
    'If founding problem is dead, dueling''s disappearance is explained by genuine problem-solving (constraint type: rope -> piton transition as function atrophies). If founding problem is zombie-persistent, institutional substitution is incomplete, and dueling might resurge if institutions fail. Classification would shift toward snare (coerced use of inferior institutional mechanism despite real underlying problem unsolved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_vs_zombie_persistence, empirical, 'Whether the honor/status/credit problem dueling solved is genuinely extinct or zombie-persistent.').

omega_variable(
    reading_vs_sibling_decomposition,
    'Is the institutional_displacement_reading a distinct causal mechanism from contraction_reading and overdetermined_composite_reading, or does actual dueling decline conflate all three?',
    'Construct counterfactuals: (1) What if institutional alternatives had NOT matured (institutional barriers remained high)? Would cultural change alone have ended dueling? (2) What if cultural axioms had NOT shifted (honor remained central)? Would institutional substitution alone have ended dueling? (3) What if both had occurred but prohibition had been unenforced? Would dueling persist at higher frequency? The answers distinguish the three readings empirically.',
    'If institutional alternatives are necessary (remove them, dueling persists even with cultural shift), this reading is primary. If cultural axioms are necessary (even with institutions available, if honor is still central, dueling continues), contraction_reading is primary. If all three are necessary independently, overdetermined_composite_reading is correct and this reading is incomplete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_decomposition, conceptual, 'Whether institutional displacement is a distinct causal mechanism or part of an overdetermined system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement_basis(duel_tr_t1700, projected).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.06).
narrative_ontology:measurement_basis(duel_tr_t1750, projected).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement_basis(duel_tr_t1800, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1875, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1875, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1875, observed).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement_basis(duel_be_t1700, projected).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement_basis(duel_be_t1750, projected).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement_basis(duel_be_t1800, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.19).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1875, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1875, 0.18).
narrative_ontology:measurement_basis(duel_be_t1875, observed).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement_basis(duel_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement_basis(duel_su_t1700, projected).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.08).
narrative_ontology:measurement_basis(duel_su_t1750, projected).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement_basis(duel_su_t1800, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1875, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1875, 0.22).
narrative_ontology:measurement_basis(duel_su_t1875, observed).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement_basis(duel_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The dueling_disappearance_mechanism kernel has three constraint readings: institutional_displacement_reading (this file) frames the mechanism as outcompetition by superior institutional alternatives; contraction_reading frames it as axiom replacement (honor → dignity culture); overdetermined_composite_reading treats all causal factors as independently sufficient. These are one kernel with three readings, not three independent constraints. Each instantiates a different ε, different beneficiary/victim structure, and different type classification based on the reading's core premise. They are linked via network.affects_constraints because they are sibling readings contesting the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
