% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Reading of the Interpretive Kernel
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the popular constitutionalism reading of the US
 *   constitutional interpretive kernel: the claim that constitutional meaning
 *   is authoritatively shaped through political struggle, social movement
 *   mobilization, and legislative action — not solely, or even primarily,
 *   through judicial pronouncement. This reading is contested by two siblings
 *   authored as separate constraints: the originalist reading (meaning fixed
 *   at ratification, authority from fidelity to framers' intent) and the
 *   living constitution reading (meaning evolves via reasoned judicial
 *   adaptation). All three readings share the same kernel — the practice of
 *   treating the constitutional text as an authoritative, contested reference
 *   point for legitimate governmental action — but instantiate structurally
 *   distinct claims about where interpretive authority actually sits. Only
 *   this reading treats extra-judicial political victory as itself
 *   constitution-making.
 *
 * KEY AGENTS:
 *   - social_reform_movements: primary beneficiary/agenda_setter (organized/mobile) — gains standing to claim constitutional authorship through mobilization
 *   - legislative_majorities: beneficiary/agenda_setter (institutional/mobile) — treated as coequal constitutional interpreters
 *   - counter_majoritarian_minorities: primary target (powerless/trapped) — loses guaranteed judicial backstop, protection becomes contingent on political strength
 *   - judicial_finality_advocates: target (institutional/constrained) — institutional authority devalued
 *   - constitutional_law_scholars: analytical observer — documents the historical record without holding power over the outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Reading of the Interpretive Kernel").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '6750ad17-1e1b-4781-ab3e-cde9a651cbdd').
narrative_ontology:cs_kernel_codification('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', fixed_text).
narrative_ontology:cs_authority_grounding('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', distributed).
narrative_ontology:cs_reading_relation('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', us_constitution_interpretive__living_constitution_reading, influences).
narrative_ontology:cs_axiom('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', foundational, interpretive_authority_shared_across_branches_and_polity).
narrative_ontology:cs_axiom_status(interpretive_authority_shared_across_branches_and_polity, holdable).
narrative_ontology:cs_axiom_grounding('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', interpretive_authority_shared_across_branches_and_polity, conventional).
narrative_ontology:cs_axiom('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', foundational, sustained_political_mobilization_can_constitute_valid_constitutional_change).
narrative_ontology:cs_axiom_status(sustained_political_mobilization_can_constitute_valid_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', sustained_political_mobilization_can_constitute_valid_constitutional_change, empirically_contingent).
narrative_ontology:cs_axiom('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', secondary, judicial_pronouncements_are_provisional_pending_political_ratification).
narrative_ontology:cs_axiom_status(judicial_pronouncements_are_provisional_pending_political_ratification, holdable).
narrative_ontology:cs_axiom_grounding('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', judicial_pronouncements_are_provisional_pending_political_ratification, conventional).
narrative_ontology:cs_reference_frame('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', departmentalist_founding_settlement).
narrative_ontology:cs_drift_state('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', post_cooper_v_aaron_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6750ad17-1e1b-4781-ab3e-cde9a651cbdd', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, social_reform_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, electoral_coalitions_seeking_realignment).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_political_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, parties_requiring_stable_settled_rights).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, long_term_institutional_planners).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, departmentalism_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_moments_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize sustained political pressure — marches, litigation campaigns, electoral organizing, sit-ins — to force constitutional meaning to shift outside or ahead of judicial doctrine (abolition, suffrage, civil rights, marriage equality all moved this way before or alongside courts). Under this reading, their sustained political success itself constitutes constitutional change, not merely a precursor to it. Exit from the constitutional order is not sought; the goal is capturing its meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, social_reform_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, social_reform_movements, agenda_setter).

% Enact statutes and structural reforms that embody a contested constitutional reading, sometimes anticipating or defying judicial doctrine (Reconstruction Congress, New Deal Congress). Under popular constitutionalism they are treated as legitimate constitutional interpreters coequal with courts, not merely ordinary policymakers awaiting judicial blessing. Their exit option is winning the next election and legislating again.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter).

% Frame judicial supremacy itself as an antidemocratic usurpation by an unelected elite and argue that constitutional meaning should track popular will expressed through elections, referenda, and mass mobilization. They benefit whenever a judicial doctrine can be recast as illegitimate elite imposition subject to popular override.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_political_claimants, beneficiary,
    organized, biographical, mobile, national).

% Depend on courts enforcing fixed rights against transient majorities — protection from majoritarian tyranny is the whole point of having rights insulated from ordinary political contestation. Under this reading, their protections become contingent on sustained political mobilization rather than judicial guarantee; if their movement loses momentum or numbers, the reading offers them no backstop. They cannot exit the polity and cannot always out-organize a hostile majority.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities, payer,
    powerless, biographical, trapped, national).

% Believe courts must have the last word to prevent constitutional meaning from dissolving into raw political power contests, and that judicial review exists precisely to remove certain questions from majoritarian revision. This reading directly denies their claim to interpretive finality, treating judicial pronouncements as one input among several rather than dispositive. Their institutional authority and professional identity are structurally devalued by the reading's success.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Businesses, property holders, contracting parties, and individuals who have structured long-term plans around settled constitutional doctrine bear the cost when meaning becomes perpetually contestable through political mobilization rather than fixed by adjudication. Settlement itself has value to them independent of which side wins; this reading treats settlement as always provisional and re-openable by sufficient political will.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, parties_requiring_stable_settled_rights, payer,
    moderate, biographical, constrained, national).

% Their institutional role as final constitutional arbiter is precisely what this reading contests; their own account of judicial supremacy (Cooper v. Aaron and its progeny) is treated by popular constitutionalists as self-serving doctrine rather than settled constitutional structure. They have no seat in the political-mobilization contest this reading valorizes — their authority comes from a different, competing source that this reading subordinates.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court_justices, excluded,
    institutional, generational, constrained, national).

% Study and debate whether constitutional change historically tracks judicial doctrine or precedes/exceeds it (Ackerman's constitutional moments, Kramer's popular constitutionalism, Post & Siegel's democratic constitutionalism). They document the historical record without themselves holding either judicial or popular-mobilization power over outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which constitutional meaning can be updated or contested when a critical mass of political actors believes existing judicial doctrine has become illegitimate or out of step with democratic will, without requiring formal Article V amendment — coordinating large-scale political mobilization into recognized constitutional change.
% TRANSFER_FUNCTION: Moves interpretive authority from courts (and from parties who rely on judicial insulation from majoritarian pressure) to organized political movements and legislative majorities; moves the practical protection of contested rights from fixed judicial guarantee toward contingent, renewable political mobilization.
% ABSENT_VOICES: Supreme Court justices and doctrinal formalists have no standing within the mobilization contest this reading valorizes — their institutional claim to finality is treated as an object of contestation rather than a starting premise. Future minorities not yet organized, or too small ever to mobilize a winning coalition, have no voice in a framework that measures legitimacy by demonstrated political strength.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism's claim were fully repudiated in favor of pure judicial supremacy, social movements would lose their strongest historical and theoretical warrant for treating extra-judicial political victories (Reconstruction Amendments' ratification politics, the New Deal settlement, civil rights legislation preceding some judicial doctrine) as constitutionally authoritative in their own right; legislative majorities acting on contested constitutional theories would be recast as merely awaiting judicial validation, reshaping how movements allocate resources between litigation and mobilization.
% FOUNDING_PROBLEM: The problem of reconciling a written, hard-to-amend constitution with the fact that its actual authoritative meaning has historically been forged through political struggle, social movements, and electoral realignments (Reconstruction, the New Deal, the civil rights era) at least as much as through judicial opinions — and the worry that pure judicial supremacy insulates courts from democratic accountability for their own constitutional errors (Dred Scott, Lochner, Plessy).
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside constitutional law's judicial-supremacy tradition (Bruce Ackerman's constitutional moments scholarship, historical accounts of the abolition and suffrage movements) corroborate that major constitutional shifts have in fact tracked political mobilization independent of or ahead of doctrine. Sitting judges and doctrinal formalists dispute the framing, arguing that whatever the historical sociology, courts alone issue binding constitutional law; this dispute is the live contest the reading names, not settled fact.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate) reflecting that this reading redistributes real protective value away from parties who depend on judicial insulation toward parties who can mobilize political power — a genuine transfer, not merely a coordination gain, even though the reading also solves a real problem (accountability of courts for their own historical failures). Suppression is moderate (0.4): the reading does not primarily operate through coercion but through delegitimizing judicial finality claims and elevating mobilization outcomes, which indirectly suppresses minorities who cannot out-organize a hostile majority. Accessibility collapse is comparatively low (0.35) because alternative interpretive theories (originalism, living constitutionalism) remain fully available and actively contest this reading in real institutions — this is a live three-way fight, not a settled monopoly. Resistance is high (0.7): judicial-supremacy institutions, formalist scholars, and doctrine-dependent parties actively contest the popular-constitutionalism claim in courts, legal academia, and political discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (social movements, legislative majorities, anti-elitist claimants) are structurally positioned to gain interpretive authority and legitimacy for extra-judicial political victories — their directionality sits toward the subsidized end. Victims (counter-majoritarian minorities, judicial finality advocates, settlement-dependent parties) lose a fixed, judicially-guaranteed backstop and instead face a contingent, renewable political contest for the same protections — their directionality sits toward the extracted end. The powerless minority seat is the most exposed: trapped exit options mean they cannot escape jurisdictions or timeframes in which their protections depend on sustained political mobilization they may lack the numbers to win.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling democratic accountability with judicial constitutional error (Dred Scott, Lochner, Plessy) — remains genuinely live: courts continue to issue contested constitutional rulings that face political backlash and eventual reversal through mobilization (e.g., post-Dobbs organizing). This is not a case of an arrangement persisting past its function; the tension between judicial finality and democratic contestation is a structural, recurring feature of constitutional government, not a solved problem being milked. The tangled_rope classification (rather than pure rope) reflects that the coordination function genuinely exists (legitimating extra-judicial constitutional change, holding courts accountable) but rides alongside a real, asymmetric cost imposed on minorities who lose guaranteed judicial protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_constitutionalism_or_political_capture,
    'Is popular constitutionalism a genuine account of how constitutional legitimacy actually operates in a democracy, or is it a post-hoc theoretical justification deployed selectively by whichever political coalition currently controls mobilization capacity to delegitimize adverse judicial rulings?',
    'Historical pattern analysis of when popular-constitutionalist arguments are invoked: consistently by whichever side is losing in courts (suggesting instrumental deployment) versus principled invocation regardless of which side benefits (suggesting genuine theory). Track invocation across both left and right mobilizations over multiple decades.',
    'If instrumentally deployed, the reading functions primarily as a delegitimation tool for whoever currently lacks judicial power, making its extraction closer to raw political capture; if genuinely principled, the coordination function (democratic accountability for judicial error) is more robust and the tangled_rope''s coordination component is stronger relative to its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_constitutionalism_or_political_capture, conceptual, 'Whether popular constitutionalism is genuine theory or opportunistic delegitimation tool.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where precisely does this reading''s disagreement with the sibling readings live: is it a disagreement about WHO has final interpretive authority (institutional question) or about WHETHER constitutional meaning is fixed versus evolving (semantic/hermeneutic question)?',
    'Distinguish cases where popular constitutionalism agrees with originalism on textual meaning but disputes judicial monopoly on enforcing it, from cases where it agrees with living constitutionalism on evolving meaning but disputes that courts alone should drive the evolution. Map historical episodes (Reconstruction, New Deal, civil rights) against both axes.',
    'If the disagreement is purely institutional (who decides), popular constitutionalism could in principle combine with either originalist or living-constitutional semantics, making it structurally orthogonal to those readings rather than a full rival account of meaning; if it is also a semantic claim (meaning is constituted by political struggle itself, not merely enforced by it), it is a fuller rival and the three-reading kernel structure is correctly triangular.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the reading disagreement is institutional (who decides) or semantic (what fixes meaning).').

omega_variable(
    minority_protection_tradeoff,
    'Does the historical record show that popular-constitutionalist episodes (Reconstruction, New Deal, civil rights) actually improved outcomes for counter-majoritarian minorities on net, or does the theory''s benefit to some minorities (Black Americans in Reconstruction and civil rights) obscure vulnerability created for others (unpopular minorities without comparable mobilization capacity)?',
    'Comparative case study across multiple minority groups'' constitutional trajectories under periods of high popular-constitutionalist activity versus high judicial-supremacy stability, controlling for group mobilization capacity.',
    'If popular constitutionalism systematically favors minorities capable of mass mobilization over those that are not, the victim declaration (counter_majoritarian_minorities) should be refined to distinguish mobilization-capable from mobilization-incapable minority groups, which would change the χ computation for that stakeholder seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_tradeoff, empirical, 'Whether the reading''s minority-protection tradeoff is uniform or depends on mobilization capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1857, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1857, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1857, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t1857, observed).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t1937, observed).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t1965, observed).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t1990, observed).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t2010, observed).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(us_c_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1857, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1857, 0.6).
narrative_ontology:measurement_basis(us_c_be_t1857, observed).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement_basis(us_c_be_t1937, observed).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement_basis(us_c_be_t1965, observed).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement_basis(us_c_be_t1990, observed).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement_basis(us_c_be_t2010, observed).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(us_c_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_interpretive__popular_constitutionalism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the US constitutional interpretive kernel,' per the ε-invariance principle: measuring interpretive authority by 'political mobilization outcome' versus 'original public meaning' versus 'reasoned judicial adaptation' yields structurally distinct ε values, beneficiary/victim sets, and classifications, so each reading is authored as an independent file. This file (popular_constitutionalism_reading) is linked bidirectionally to originalist_reading and living_constitution_reading via affects_constraints; each sibling should reciprocally link back here. This reading's success or failure in a given political moment directly affects the resources and legitimacy available to the other two readings (e.g., a period of high popular mobilization success reduces the practical authority living_constitution_reading's judicial-adaptation mechanism claims for itself, and directly contests originalist_reading's claim that ratification-era meaning is dispositive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
