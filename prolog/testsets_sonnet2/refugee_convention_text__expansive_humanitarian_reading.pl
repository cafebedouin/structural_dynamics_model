% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: 1951 Refugee Convention — Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story authors the expansive humanitarian reading of the Refugee
 *   Convention text as its own constraint, distinct from the
 *   restrictive-sovereignty and procedural-integrity readings of the same
 *   kernel. Under this reading, the Convention operates as an unbendable
 *   humanitarian floor: 'well-founded fear' extends to generalized violence
 *   and non-state persecution, and 'particular social group' extends to
 *   gender, sexual orientation/gender identity, and clan membership. The
 *   reading genuinely coordinates protection for populations the 1951
 *   drafters likely did not contemplate, but it also imposes a substantial
 *   and growing enforcement burden on destination states and first-line
 *   responders who did not choose the interpretive expansion and experience
 *   it as externally imposed law. That asymmetry — real coordination benefit
 *   for claimants, real cost imposed on adjudicating states and local
 *   responders, sustained only through active tribunal enforcement and
 *   treaty-body pressure — is why this reading is authored as tangled_rope
 *   rather than a pure rope or a pure snare.
 *
 * KEY AGENTS:
 *   - gender_persecution_claimants: primary beneficiary (powerless/trapped) — gains a legal category that did not clearly exist for them under a narrower reading
 *   - lgbtq_asylum_seekers: primary beneficiary (powerless/trapped) — gains protection recognition contingent on which reading a tribunal adopts
 *   - clan_based_persecution_survivors: primary beneficiary (powerless/trapped) — gains recognition of generalized/diffuse violence as qualifying fear
 *   - unhcr_protection_mandate: agenda-setter (institutional/analytical) — advances and administers the interpretive expansion without coercive enforcement power
 *   - destination_state_border_agencies: primary payer (institutional/constrained) — bears adjudication burden and loses interdiction/offshore tools
 *   - restrictive_sovereignty_states: excluded objector — voices a competing textual reading not treated as authoritative within this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.42).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.55).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "1951 Refugee Convention — Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '620a8b93-e70d-4749-9933-dc094124bf70').
narrative_ontology:cs_kernel_codification('620a8b93-e70d-4749-9933-dc094124bf70', fixed_text).
narrative_ontology:cs_authority_grounding('620a8b93-e70d-4749-9933-dc094124bf70', expertise).
narrative_ontology:cs_interpretation_layer_present('620a8b93-e70d-4749-9933-dc094124bf70').
narrative_ontology:cs_reading_relation('620a8b93-e70d-4749-9933-dc094124bf70', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('620a8b93-e70d-4749-9933-dc094124bf70', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('620a8b93-e70d-4749-9933-dc094124bf70', foundational, protection_scope_must_track_actual_harm_not_persecutor_identity).
narrative_ontology:cs_axiom_status(protection_scope_must_track_actual_harm_not_persecutor_identity, holdable).
narrative_ontology:cs_axiom_grounding('620a8b93-e70d-4749-9933-dc094124bf70', protection_scope_must_track_actual_harm_not_persecutor_identity, deontological).
narrative_ontology:cs_axiom('620a8b93-e70d-4749-9933-dc094124bf70', foundational, convention_terms_evolve_with_understanding_of_human_rights).
narrative_ontology:cs_axiom_status(convention_terms_evolve_with_understanding_of_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('620a8b93-e70d-4749-9933-dc094124bf70', convention_terms_evolve_with_understanding_of_human_rights, instrumental).
narrative_ontology:cs_axiom('620a8b93-e70d-4749-9933-dc094124bf70', secondary, state_consent_at_ratification_bounds_permissible_interpretation).
narrative_ontology:cs_axiom_status(state_consent_at_ratification_bounds_permissible_interpretation, overridden).
narrative_ontology:cs_axiom_grounding('620a8b93-e70d-4749-9933-dc094124bf70', state_consent_at_ratification_bounds_permissible_interpretation, conventional).
narrative_ontology:cs_reference_frame('620a8b93-e70d-4749-9933-dc094124bf70', post_war_state_persecution_paradigm).
narrative_ontology:cs_drift_state('620a8b93-e70d-4749-9933-dc094124bf70', contemporary_generalized_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('620a8b93-e70d-4749-9933-dc094124bf70', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_survivors).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_advocacy_networks).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_state_border_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, host_community_first_responders).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, detained_transit_populations).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_as_jus_cogens).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, convention_as_living_instrument_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee domestic violence regimes, forced marriage, or honor-based violence that home states decline to prevent or punish. Under this reading, the Convention's 'particular social group' ground extends to them even though their persecutor is often a private individual rather than the state itself. Without this reading, their claims are frequently rejected as 'private harm' outside Convention scope; their survival depends on which reading a given tribunal adopts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Face criminalization, mob violence, or family-sanctioned killing in countries where authorities either enact or tolerate the persecution. This reading recognizes sexual orientation and gender identity as an immutable or fundamental-to-identity social group regardless of state complicity. They have essentially no alternative legal pathway to protection if this reading is not adopted by the deciding jurisdiction.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Flee generalized clan or ethnic violence in contexts of state collapse (e.g., failed-state civil conflict) where no single persecutor can be individually named and the violence is diffuse rather than targeted at them personally. This reading holds that generalized violence satisfies 'well-founded fear' when it falls disproportionately on their group; a narrower reading would require them to prove individualized targeting they cannot document while fleeing collapse.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_survivors, beneficiary,
    powerless, biographical, trapped, regional).

% Publishes interpretive guidelines urging states to read 'particular social group' and 'well-founded fear' broadly, monitors state compliance, and advocates against interdiction and offshore processing as constructive refoulement. Its institutional legitimacy and continued relevance are bound up with the expansive reading prevailing; it has no coercive enforcement power of its own and depends on states' voluntary adoption of its interpretive guidance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, agenda_setter,
    institutional, generational, analytical, global).

% NGOs and litigation networks build legal strategy, precedent, and public pressure around the expansive reading. They gain institutional standing, funding, and case law wins from its adoption; their organizational mission is structurally tied to the reading's continued advance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_advocacy_networks, agenda_setter).

% Must substantively adjudicate a dramatically larger and more heterogeneous claim pool, in individualized hearings, without categorical fast-track denial for generalized-violence or non-state-persecutor claims. Backlogs grow; agencies are enjoined by domestic courts from operating interdiction or offshore-processing regimes that this reading treats as refoulement. They experience the reading as an externally imposed mandate they did not choose and cannot easily modify without withdrawing from or reserving against the Convention.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_state_border_agencies, payer,
    institutional, immediate, constrained, national).

% Local municipalities, shelters, and social services absorb the practical caseload increase generated by a broader eligible population, often without proportionate resourcing from national governments, since the expansion is judicially or treaty-driven rather than budget-planned.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, host_community_first_responders, payer,
    moderate, immediate, trapped, local).

% Held in transit or offshore facilities while states resist the expansive reading's implications by slowing processing, contesting jurisdiction, or maintaining detention as deterrence; the contest over the reading itself prolongs their limbo even when the reading, if applied, would ultimately favor them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, detained_transit_populations, payer,
    powerless, immediate, trapped, regional).

% Would object that this reading substitutes UNHCR and tribunal interpretive expansion for the treaty text states actually ratified, effectively amending the Convention without renegotiation. Their objection is voiced in state practice (reservations, non-compliance, domestic legislative override) but is not treated as authoritative within this reading's own framework, which regards such resistance as non-compliance rather than valid counter-interpretation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_states, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared international floor of protection so that people fleeing severe harm are not returned to it, and so that no single state bears the full cost of interpreting ambiguous treaty terms alone — a common interpretive standard lets refugees, states, and adjudicators predict outcomes across borders.
% TRANSFER_FUNCTION: Moves the burden of protection and its associated costs (housing, adjudication capacity, integration services) from states of origin (which caused or permitted the harm) toward destination and transit states, and moves interpretive authority from individual sovereign legislatures toward supranational guidance bodies and international tribunals.
% ABSENT_VOICES: Restrictive-sovereignty states and their electorates are structurally present as objectors in international fora but are treated, within this reading's own logic, as non-compliant rather than as holders of a legitimate competing interpretation; their objection is heard but not weighted as authoritative inside the expansive framework.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned tomorrow, gender, LGBTQ+, clan-based, and generalized-violence claimants would lose their primary legal basis for protection in many jurisdictions overnight; interdiction and offshore processing regimes currently constrained by non-refoulement obligations under this reading would expand; case law built over decades in UK, Canadian, and EU tribunals recognizing these categories would be vulnerable to reversal.
% FOUNDING_PROBLEM: The 1951 Convention was drafted in the aftermath of WWII to prevent a repeat of states refusing entry to people fleeing state-sponsored persecution (the paradigm case being flight from Nazi Germany), when the drafters' contemplated persecutor was overwhelmingly the state itself acting against political, religious, or ethnic dissenters.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and refugee law scholars (an interested constituency) attest the founding humanitarian purpose demands evolutionary interpretation to cover forms of persecution unimagined in 1951. Independent evidence outside the benefiting advocacy network is mixed: comparative treaty-interpretation scholars (e.g., work applying the Vienna Convention's ordinary-meaning rule) and several national supreme courts have found the expansive reading defensible on textual grounds independent of advocacy pressure, while other national courts and several ratifying states' own travaux-préparatoires-based arguments dispute that the drafters intended non-state-actor or generalized-violence coverage — so corroboration exists on both sides and the founding-intent question itself remains unsettled outside the beneficiary set.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 — this reading does move real costs (adjudication capacity, detention alternatives, local service burden) from claimants to states and host communities, but it is not a naked extraction mechanism; it rides a genuine coordination function (a shared floor against refoulement). Suppression climbs over the interval (0.20 to 0.55) as case law hardens the expansive categories into binding precedent and treaty-monitoring bodies increasingly treat departure from it as non-compliance rather than legitimate alternative interpretation — this is the enforcement intensification the tangled_rope classification requires. Theater ratio rises modestly (0.10 to 0.28) as some state 'compliance' with the expansive reading becomes procedural box-ticking (nominal individualized hearings that functionally rubber-stamp categorical outcomes) rather than substantive engagement with each claim. Accessibility collapse is authored low-moderate (0.35): the restrictive and procedural readings remain live, actively litigated alternatives in many jurisdictions, so alternatives to this reading have not collapsed. Resistance is authored high (0.72): the reading is fiercely contested in domestic legislatures, judiciaries, and among sovereignty-focused states — this is not a settled mountain-like consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Gender, LGBTQ+, and clan-based claimants are declared beneficiaries and are structurally trapped (they cannot exit the persecution context and depend entirely on which reading is applied to their case), which the derivation chain should push toward the strong-beneficiary end of directionality despite their powerless power atom — the constraint, when it applies, subsidizes their survival. Destination-state border agencies and host-community responders are declared payers with institutional/moderate power but constrained/trapped exit respectively (an institutional agency cannot unilaterally withdraw from treaty obligations; a municipality cannot refuse arriving claimants), pushing them toward the target end. UNHCR sits close to symmetric-but-agenda-setting: it administers and advocates for the reading but does not itself bear the fiscal cost or receive the protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state persecution of political/religious/ethnic dissenters, paradigmatically 1930s-40s Europe) is authored as contested rather than dead: the mechanism of state-organized persecution the Convention was built for still occurs, but this reading extends the same instrument to categories (private violence, non-state actors, generalized conflict violence) the founding problem did not encompass. This is not mandatrophy in the classic 'dead problem, persistent institution' sense — it is closer to institutional evolution under contested legitimacy. Classifying it as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the expansion as pure extraction (it does protect real people from real harm) and treating it as settled natural law (it remains actively contested by ratifying states themselves, which a mountain classification would obscure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_originalism_vs_living_instrument,
    'Does the ordinary-meaning/travaux-préparatoires evidence support reading ''particular social group'' and ''well-founded fear'' as extending to non-state persecutors and generalized violence, or does it confine the terms to the state-persecution paradigm the 1951 drafters had in view?',
    'Systematic comparative analysis of travaux préparatoires alongside decades of state practice and subsequent treaty-body interpretation (VCLT Article 31(3)(b) subsequent practice test) across multiple ratifying jurisdictions.',
    'If travaux and consistent state practice support only the narrow reading, this reading''s claim to being the authoritative interpretation of the same text (rather than a policy preference dressed as interpretation) weakens substantially, strengthening the restrictive_sovereignty_reading''s competing claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_originalism_vs_living_instrument, conceptual, 'Whether the expansive reading is genuine treaty interpretation or judicial/advocacy-driven amendment of the Convention''s original scope.').

omega_variable(
    cost_shifting_versus_genuine_protection_gap,
    'Is the burden imposed on destination states and host communities proportionate to a genuine protection gap left open by narrower readings, or does it reflect over-extension of categories beyond what the underlying humanitarian rationale requires?',
    'Empirical study comparing outcomes (recognition rates, false-positive/false-negative rates, downstream integration success) across jurisdictions applying expansive versus restrictive readings to matched claimant profiles.',
    'If recognition-rate divergence tracks genuine unmet need, the coordination function of this reading is strongly vindicated; if divergence mainly reflects doctrinal drift without corresponding need differences, the tangled_rope''s extractive component is larger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_shifting_versus_genuine_protection_gap, empirical, 'Whether the cost burden this reading imposes on states is proportionate to genuine unmet protection need.').

omega_variable(
    sovereignty_objection_as_bad_faith_or_legitimate_dissent,
    'Is state resistance to the expansive reading better characterized as bad-faith evasion of binding humanitarian obligations, or as a legitimate competing interpretation of an ambiguous, multiply-ratified treaty text that this reading treats as illegitimate by definitional fiat?',
    'Track whether objecting states'' domestic courts, when independent of executive migration policy, converge toward or diverge from the expansive reading over time — convergence would suggest the objection is policy-driven rather than genuinely interpretive.',
    'If domestic judiciaries independently converge toward expansive readings despite executive resistance, that corroborates this reading''s claim to interpretive authority; persistent independent judicial divergence would support treating the restrictive_sovereignty_reading as a legitimate rather than merely non-compliant alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_objection_as_bad_faith_or_legitimate_dissent, conceptual, 'Whether characterizing sovereignty-based resistance as non-compliance (rather than valid alternative interpretation) is itself doing interpretive work that favors this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.15).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.34).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2005, 0.43).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the single natural-language label 'the Refugee Convention' per the ε-invariance principle: expansive_humanitarian_reading (this file, ε=0.42, tangled_rope), restrictive_sovereignty_reading (lower ε expected, narrower beneficiary set), and procedural_integrity_reading (distinct ε centered on process rather than substantive outcome breadth). Each reading is authored as a separate constraint with its own beneficiary/victim structure because measuring the same treaty text through the lens of each reading yields materially different extraction profiles — the readings are not the same constraint viewed from different angles, they are structurally distinct constraints sharing a textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
