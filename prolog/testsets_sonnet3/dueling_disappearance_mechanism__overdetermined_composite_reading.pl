% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Account of Dueling's Decline (1840s-1890s Anglo-American Honor Culture)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the overdetermined-composite reading of the
 *   dueling-disappearance kernel: dueling's collapse in the Anglo-American
 *   world (roughly 1840s-1890s) is explained not by any single sufficient
 *   cause but by the simultaneous, independently-operating action of legal
 *   prohibition (state statutes and civil disability clauses), institutional
 *   modernization (courts, formal libel/slander remedies, insurance and
 *   banking risk-pricing), cultural dignity-shift (treated here as one input
 *   among several, not the master cause), and Civil War trauma (the war
 *   discredited the martial honor ethos that had underwritten dueling,
 *   especially in the South). Because the causal pathways operated
 *   concurrently and are not cleanly separable in the historical record, no
 *   single mechanism-specific ε is authored here — extraction is measured as
 *   the composite operation of institutions and classes that jointly absorbed
 *   the reputational-defense function dueling had performed, at cost to those
 *   whose standing depended on the old ritual and its increasingly
 *   friendless. This reading is one of three in the
 *   dueling_disappearance_mechanism kernel; the sibling readings
 *   (contraction_reading: pure cultural/dignity-culture displacement;
 *   institutional_displacement_reading: pure institutional substitution) are
 *   separate constraint files with their own ε and stakeholder structures,
 *   not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - state_judicial_authorities: agenda_setter (institutional/arbitrage) — administers legal prohibition and absorbs dispute-resolution jurisdiction
 *   - emerging_professional_class: beneficiary (organized/mobile) — gains status allocation as honor shifts to credentialing
 *   - life_insurance_and_banking_institutions: beneficiary (organized/mobile) — gains actuarially safer subjects
 *   - postbellum_southern_reconciliation_elites: beneficiary (powerful/constrained) — uses dueling's discrediting to signal modernization
 *   - displaced_honor_code_gentry: payer (moderate/trapped) — loses primary status-defense mechanism with no substitute
 *   - dueling_seconds_and_code_duello_arbiters: payer (powerless/trapped) — specialized role becomes worthless
 *   - historians_of_the_composite_thesis: observer (analytical) — traces converging, non-monocausal timelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.55).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Composite Account of Dueling's Decline (1840s-1890s Anglo-American Honor Culture)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd').
narrative_ontology:cs_kernel_codification('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', distributed).
narrative_ontology:cs_authority_grounding('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', distributed).
narrative_ontology:cs_reading_relation('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', foundational, causal_pathways_are_jointly_sufficient_and_nonseparable).
narrative_ontology:cs_axiom_status(causal_pathways_are_jointly_sufficient_and_nonseparable, holdable).
narrative_ontology:cs_axiom_grounding('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', causal_pathways_are_jointly_sufficient_and_nonseparable, empirically_contingent).
narrative_ontology:cs_axiom('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', secondary, no_single_mechanism_bears_explanatory_priority).
narrative_ontology:cs_axiom_status(no_single_mechanism_bears_explanatory_priority, holdable).
narrative_ontology:cs_axiom_grounding('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', no_single_mechanism_bears_explanatory_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', antebellum_honor_culture_equilibrium).
narrative_ontology:cs_drift_state('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', post_civil_war_reconstruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('91cb56a2-8cc7-44d1-9dd8-0d0b49e024fd', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_professional_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_judicial_authorities).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, life_insurance_and_banking_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_southern_reconciliation_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, displaced_honor_code_gentry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_seconds_and_code_duello_arbiters).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, widows_and_dependents_of_slain_duelists_pre_decline).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, working_class_men_denied_honor_venue_substitutes).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, multicausal_social_change_thesis).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, modernization_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lawyers, doctors, merchants, and civil servants whose status depended on credentialing and institutional standing rather than martial honor. They benefited as dueling's decline reallocated prestige toward professional achievement and litigation-based reputation defense, which they were positioned to win.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_professional_class, beneficiary,
    organized, generational, mobile, national).

% State legislatures and courts that criminalized dueling, stripped duelists of civil rights (office-holding bans, disenfranchisement clauses), and simultaneously expanded civil libel and slander remedies. They administered the legal prohibition mechanism and absorbed the dispute-resolution function dueling had occupied, gaining jurisdiction and legitimacy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_judicial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Insurers wrote dueling-death exclusion clauses and banks required actuarial-grade risk profiles for credit; both benefited from a population that no longer casually risked death over insult, since it made men better financial and insurance subjects and reduced payout volatility.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, life_insurance_and_banking_institutions, beneficiary,
    organized, generational, mobile, national).

% Southern elites rebuilding social order after catastrophic Civil War losses used dueling's association with the old antebellum honor culture (and its perceived contribution to a martial ethos that led to secession) as a repudiated symbol, using its decline to signal modernization to northern capital and to distance themselves from a discredited past while consolidating a new Jim Crow social order.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_southern_reconciliation_elites, beneficiary,
    powerful, generational, constrained, regional).

% Older gentry whose entire social standing had been built on willingness to duel lost their primary mechanism for defending reputation and rank. They experienced the composite decline as a coerced loss of status with no adequate substitute institution recognizing their claims to honor.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, displaced_honor_code_gentry, payer,
    moderate, biographical, trapped, regional).

% The specialized social role of second, arbiter, and code-duello expert (who negotiated terms, verified fairness, and managed the ritual) lost its function entirely and had no successor role in the courts or clubs that replaced it. Their expertise became worthless overnight relative to the compressed timescale of the multi-cause collapse.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_seconds_and_code_duello_arbiters, payer,
    powerless, biographical, trapped, local).

% Families who bore fatal costs of the dueling system in the years before the composite decline fully took hold; they received no restitution and their losses occurred precisely during the period when multiple mechanisms were independently converging but had not yet jointly suppressed the practice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, widows_and_dependents_of_slain_duelists_pre_decline, payer,
    powerless, biographical, trapped, local).

% Non-elite men had never had full access to formal dueling (a gentry privilege) and, as dueling collapsed, gained no compensating standing in the courts or insurance economy that absorbed elite honor functions; the composite mechanisms modernized reputation defense mainly for those already positioned to use law and credit.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, working_class_men_denied_honor_venue_substitutes, payer,
    powerless, biographical, trapped, local).

% Social historians who argue against monocausal accounts (pure cultural shift, pure institutional displacement) by tracing legal statute timing, bank/insurance archives, postbellum reconciliation rhetoric, and cultural texts as jointly sufficient, independently-operating causes converging in the same three to four decades.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historians_of_the_composite_thesis, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite of legal prohibition, institutional modernization, actuarial risk-pricing, and postwar trauma jointly solved the problem of allocating dispute-resolution and reputation-defense authority away from a lethal, unregulated ritual and toward institutions (courts, insurers, professional associations) capable of absorbing and monetizing that function at scale.
% TRANSFER_FUNCTION: Moves reputational adjudication authority and its associated status rents from an informal gentry-controlled ritual (dueling, its seconds, its code) to formal institutions (courts, banks, insurers, professional bodies) controlled by an emerging class better positioned to profit from formalized, litigable, insurable dispute resolution.
% ABSENT_VOICES: The seconds and code-duello arbiters whose expertise vanished, and the working-class men who never had dueling access but also gained no substitute standing, left no organized record objecting to the transition — their absence from the historiography is itself part of why the multicausal thesis can be told largely from the perspective of the institutions that benefited from convergence.
% DISAPPEARANCE_RATIONALE: If the overdetermined-composite mechanism itself vanished as an explanatory frame (i.e., if a single dominant cause were established beyond dispute), the historiographical arrangement changes substantially — different actors' contributions to modernization narratives would be reallocated, and the postbellum reconciliation elites' framing of the war-trauma cause would lose evidentiary cover from the other three causes; whether the underlying social fact of dueling's decline itself would look different is a separate, less contested question.
% FOUNDING_PROBLEM: The composite/overdetermination account was built to solve a historiographical problem: explaining why dueling collapsed within a narrow window (roughly 1840s-1890s in the US South, earlier in the North and Britain) when no single cause (legal, cultural, institutional, or traumatic) can be shown to be independently necessary — each candidate cause co-occurs with the others and none clearly precedes and fully explains the collapse alone.
% FOUNDING_PROBLEM_CORROBORATION: Quantitative historians using statute-timing and dueling-incident count data (outside any single beneficiary group) corroborate that no single mechanism's timeline alone tracks the decline curve, supporting overdetermination; however, cultural historians aligned with the contraction/dignity-culture reading and institutionalist historians aligned with the displacement reading both dispute that overdetermination is the correct frame rather than an artifact of insufficiently disaggregated causal claims — no fully independent arbiter outside the competing historiographical schools has settled this.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).
:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is authored moderate rather than high or low because the composite mechanism redistributes reputational authority toward institutions and classes able to monetize the transition (courts collecting fees and jurisdiction, insurers repricing risk, professional classes gaining relative status) while displacing a narrower gentry class and an entirely-eliminated specialist role (seconds/arbiters) — a real but not maximal asymmetric transfer. Suppression (0.55) reflects that the composite mechanism required active legal enforcement (statutes, disenfranchisement clauses, prosecutions) layered onto softer cultural and institutional pressure — not pure voluntary abandonment. Theater ratio (0.4) captures that a meaningful share of anti-dueling legal and social activity was performative (symbolic legislation rarely enforced, public denunciations by elites who had themselves dueled) relative to functionally decisive causes. Accessibility collapse (0.6) is moderate-high: by the 1880s dueling had become nearly unthinkable as a live option for the classes it once served, though not with mountain-level completeness, since isolated duels persisted into the 1900s. Resistance (0.35) is low-moderate: displaced gentry grumbled and some duels continued defiantly, but organized resistance to the composite transition was limited and fragmented. The measurement grid runs 1800-1900 at seven shared time points; suppression_requirement peaks at 1865 reflecting the concentrated post-Civil-War legal and cultural crackdown, then eases slightly as the transition consolidates.
 *
 * PERSPECTIVAL GAP:
 *   From the state/institutional/financial seats, the composite decline reads as successful modernization: multiple reinforcing causes converged to replace a dangerous, unregulated practice with regulated, monetizable alternatives — a rope-like coordination success. From the displaced gentry and eliminated-specialist seats, the same convergence reads as an overwhelming, multi-front extraction of status and function with no single point of resistance possible, precisely because the causes were overdetermined and simultaneous — a tangled-rope or near-snare experience. The engine's per-seat computation should reflect this: the coordination function is real (institutions did solve a genuine dispute-resolution and risk problem) but the same structure imposed asymmetric, uncompensated costs on specific groups, satisfying the tangled_rope gate.
 *
 * DIRECTIONALITY LOGIC:
 *   State judicial authorities and the professional/financial classes are structural beneficiaries: they gain jurisdiction, monetizable risk-pools, and relative status without themselves bearing dueling's historical costs — d near the beneficiary end. Postbellum southern elites are beneficiaries at a regional scope, using the discredited practice instrumentally for reconciliation politics. Displaced gentry, seconds/arbiters, and pre-decline duel widows are targets: they bear the cost of the transition (lost status, eliminated role, uncompensated prior losses) with trapped exit options, since the composite mechanism's simultaneity gave no institution time to build a substitute recognizing their specific claims. Working-class men are targets in a different register — never having full access to the practice, they also gained nothing from its formalized replacement.
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermination framing itself risks mandatrophy if invoked to avoid assigning responsibility to any single institutional actor — 'everything caused it, so no one owns the transition costs' can function as a beneficiary-protecting narrative. This story treats the composite as jointly sufficient causes with jointly distributed benefits and costs rather than as a diffuse, ownerless process, which is why beneficiaries and victims are both explicitly named rather than left unspecified. The claimed tangled_rope type, not mountain, is deliberate: overdetermination explains the mechanism's causal structure, not its naturalness or inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_hidden_master_cause,
    'Is the composite/overdetermined structure genuine (multiple independently sufficient causes truly converging) or does it conceal an underlying master cause that the historical record has not yet isolated, with the other three factors merely correlated epiphenomena?',
    'Fine-grained regional and temporal disaggregation: comparing localities where one candidate cause (e.g., legal prohibition) was present without the others to see if dueling declined there alone at comparable rates. If decline tracks only when all four co-occur, overdetermination is supported; if decline tracks strongly with one factor regardless of the others, a master cause is more likely.',
    'If a master cause is isolated, this reading collapses into (or is subsumed by) one of the sibling readings, and the tangled_rope classification with its diffuse multi-beneficiary structure would need to be re-evaluated against that sibling''s cleaner beneficiary/victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_hidden_master_cause, empirical, 'Whether the four candidate causes are truly independently sufficient or one dominates.').

omega_variable(
    beneficiary_class_boundary_ambiguity,
    'Are the professional class, judicial authorities, and financial institutions genuinely distinct beneficiary groups with independent interests in the decline, or is this a single elite formation described under three names?',
    'Prosopographical analysis of overlap in membership (how many judges, bankers, and professionals were the same individuals or closely networked) across the relevant decades.',
    'High overlap would suggest the beneficiary structure is more concentrated (closer to snare) than the diffuse tangled_rope reading assumes; low overlap supports the genuinely multi-actor coordination-with-extraction structure claimed here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_boundary_ambiguity, empirical, 'Whether the named beneficiary groups are structurally distinct or a single elite network.').

omega_variable(
    cs_framing_kernel_vs_layered_narrative,
    'Should the dueling_disappearance_mechanism kernel be read as a single contested historical-causal claim (as done here, with three competing readings), or does the postbellum reconciliation narrative layered atop the raw causal claim constitute a second, distinct kernel about historical memory and legitimacy that deserves its own decomposition?',
    'Compare whether reconciliation-elite rhetoric about dueling''s decline (used to signal southern modernization to northern capital) tracks the same evidentiary basis as the raw causal-mechanism claims, or whether it operates on a distinct legitimacy-narrative logic that would classify differently (e.g., as its own tangled_rope over historical memory rather than over the causal mechanism itself).',
    'If the reconciliation-narrative layer is a distinct kernel, the postbellum_southern_reconciliation_elites beneficiary group and part of the theater_ratio measurement here should be split into a separate constraint story, changing this story''s ε and stakeholder set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_layered_narrative, conceptual, 'Whether the reconciliation-legitimacy narrative is part of this kernel or a separate, unaddressed kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.2).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.28).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.32).
narrative_ontology:measurement(duel_tr_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1865, 0.38).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.42).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.25).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.32).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.38).
narrative_ontology:measurement(duel_be_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1865, 0.45).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.32).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.42).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.48).
narrative_ontology:measurement(duel_su_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1865, 0.6).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.58).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dueling_disappearance_mechanism kernel. contraction_reading isolates cultural/dignity-culture displacement as the master mechanism (a rope/mountain-leaning claim about axiom shift). institutional_displacement_reading isolates formal institutional substitution as the master mechanism (a tangled_rope claim centered on courts/banks/libel-law outcompeting dueling). This overdetermined_composite_reading treats both candidate master-mechanisms, plus legal prohibition and Civil War trauma, as jointly and independently sufficient rather than competing, yielding a broader and more diffuse beneficiary/victim structure and a tangled_rope classification without a single cleanly separable ε per sub-mechanism. Each sibling should link back to this constraint_id in its own network.affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
