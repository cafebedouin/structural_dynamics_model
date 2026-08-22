% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Popular Sovereignty
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the 'republican reading' of legitimate
 *   authority, where power originates from the people and is delegated
 *   through consent, typically via elections and constitutional frameworks.
 *   It is grounded in popular sovereignty and social contract theory. While
 *   presented as a 'rope' by its proponents, ensuring coordination and
 *   self-governance, it carries a moderate level of extraction from those
 *   excluded from the franchise or whose voices are marginalized by
 *   majoritarian mechanisms. The historical measurements reflect the
 *   expansion of suffrage and reduction of overt suppression over time,
 *   leading to a decrease in extractiveness and suppression, though a slight
 *   recent uptick suggests new challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.3).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Popular Sovereignty").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'a69d3cc2-3006-4fc8-aeea-21d63e356fd3').
narrative_ontology:cs_kernel_codification('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', formalized).
narrative_ontology:cs_authority_grounding('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', lineage).
narrative_ontology:cs_interpretation_layer_present('a69d3cc2-3006-4fc8-aeea-21d63e356fd3').
narrative_ontology:cs_reading_relation('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', foundational, authority_derives_from_consent).
narrative_ontology:cs_axiom_status(authority_derives_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', authority_derives_from_consent, deontological).
narrative_ontology:cs_axiom('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', foundational, popular_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', popular_sovereignty_is_supreme, conventional).
narrative_ontology:cs_reference_frame('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', enlightenment_social_contract).
narrative_ontology:cs_drift_state('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', contemporary_political_polarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a69d3cc2-3006-4fc8-aeea-21d63e356fd3', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_franchise).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, minority_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The source of legitimate authority, delegating power through elections and participatory mechanisms. Benefits from self-governance but is constrained by the practicalities of collective action and the potential for majoritarian overreach.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_franchise, beneficiary,
    organized, generational, constrained, national).

% Exercise delegated authority, enacting laws and governing on behalf of the people. Their legitimacy is tied to electoral cycles and adherence to constitutional principles. They benefit from holding power but are accountable to the electorate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    institutional, biographical, constrained, national).

% Those denied voting rights or effective participatory mechanisms. They bear the costs of governance without having a direct voice in its formation, experiencing the constraint as a form of extraction or suppression of their political agency.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_populations, payer,
    powerless, generational, trapped, national).

% While possessing franchise, their interests may be systematically overridden by majoritarian rule. They pay the cost of policies they oppose and face challenges in achieving political representation or influence, experiencing a form of diffuse extraction.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, minority_factions, payer,
    moderate, biographical, constrained, national).

% Analyze the theoretical underpinnings and practical application of popular sovereignty and social contract theory. They assess the coherence and effectiveness of the republican model of legitimacy.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable and legitimate framework for collective decision-making and governance by channeling popular will into representative institutions, preventing anarchy or arbitrary rule.
% TRANSFER_FUNCTION: Transfers political authority from the collective 'people' to elected officials, in exchange for governance and the protection of rights. It also transfers the burden of compliance from the governed to the governing, who must continually seek consent.
% ABSENT_VOICES: Those historically or presently excluded from the franchise (e.g., non-citizens, certain historical demographics) would argue that the 'popular' in popular sovereignty is too narrowly defined, making the system less legitimate than claimed. Future generations, whose consent is presumed, also lack a direct voice.
% DISAPPEARANCE_RATIONALE: If the belief in legitimate authority flowing from popular consent vanished, the entire edifice of modern republican governance would collapse. Elections would lose meaning, laws would lose their moral force, and political systems would devolve into either pure power struggles or alternative, non-consensual forms of authority.
% FOUNDING_PROBLEM: To establish a stable and just form of government that avoids both the tyranny of monarchy and the chaos of direct democracy, by grounding authority in the consent of the governed.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and historians attest to the ongoing challenge of balancing popular will with minority rights and institutional stability. Contemporary political discourse and constitutional debates consistently revisit the scope and limits of popular sovereignty, indicating the problem remains live. International human rights organizations also corroborate the ongoing struggle for universal franchise and political participation.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the inherent costs of collective decision-making and the potential for majoritarian rule to impose costs on minorities, as well as the historical exclusion of certain groups. Suppression (0.30) is present in the mechanisms that define and limit who counts as 'the people' and how their consent is expressed, though it has decreased significantly over time with the expansion of democratic rights. The theater ratio (0.10) is low, indicating that the mechanisms of consent and representation are largely functional, though there can be performative aspects in political campaigns. Accessibility collapse (0.60) is moderate; while alternatives to republicanism exist, the established system makes them difficult to realize. Resistance (0.25) is present from excluded groups and those advocating for deeper forms of democracy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry with franchise, this is a legitimate and beneficial 'rope' ensuring self-governance. From the perspective of excluded populations, it operates as a 'snare' or 'tangled rope' that extracts compliance without genuine consent. Elected representatives view it as a 'rope' that grants them authority, while minority factions may experience it as a 'tangled rope' due to majoritarian tyranny. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with franchise and elected representatives are beneficiaries, as they directly participate in and benefit from the self-governance framework. Excluded populations and minority factions are payers, as they bear the costs of a system where their consent is either not sought or their interests are systematically underrepresented. The directionality for excluded populations is high (near 1.0), while for minority factions it is moderate (closer to 0.7-0.8), reflecting their partial inclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not currently experiencing mandatrophy. The founding problem of establishing legitimate governance through consent remains 'live', and the mechanisms of popular sovereignty, while imperfect, are still actively engaged. The slight increase in extractiveness and suppression in recent years suggests new challenges to the ideal of popular sovereignty, rather than an atrophy of its function. The classification as 'rope' (claimed) with moderate extraction (metrics) prevents mislabeling it as pure extraction, acknowledging its genuine coordination function, while also highlighting its costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_the_people,
    'Who constitutes ''the people'' whose consent grants legitimacy, and how is this definition enforced or contested?',
    'Historical analysis of suffrage expansion, legal challenges to voting rights, and sociological studies of political inclusion/exclusion.',
    'If ''the people'' is narrowly defined and actively policed, the constraint''s effective suppression and extractiveness are higher, potentially shifting its classification towards a ''snare'' for excluded groups. If broadly inclusive, it reinforces the ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_the_people, empirical, 'Ambiguity in the definition of ''the people'' in popular sovereignty.').

omega_variable(
    consent_mechanism_authenticity,
    'To what extent do electoral and participatory mechanisms genuinely reflect the ''delegated consent'' of the people, versus being subject to manipulation or apathy?',
    'Empirical studies of electoral integrity, voter turnout, political campaign finance, and public trust in institutions.',
    'If consent mechanisms are found to be systematically compromised or performative, the ''theater_ratio'' would increase, and the ''extractiveness'' from the citizenry would be higher, as their consent is presumed rather than genuinely given. This could shift the classification towards a ''piton'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_authenticity, empirical, 'Authenticity of consent mechanisms in republican governance.').

omega_variable(
    majoritarian_tyranny_threshold,
    'At what point does majoritarian rule, even if based on consent, become a form of ''tyranny of the majority'' that extracts from or suppresses minority rights, thereby undermining its own legitimacy?',
    'Legal and philosophical analysis of constitutional protections for minorities, historical case studies of majoritarian overreach, and normative debates on the limits of popular sovereignty.',
    'If the threshold for majoritarian tyranny is low or frequently crossed, the ''extractiveness'' from minority factions would be re-evaluated as higher, and the ''suppression'' of their political agency more severe, potentially reclassifying the constraint as a ''tangled_rope'' for these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_threshold, conceptual, 'The boundary between legitimate majoritarian rule and majoritarian tyranny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(sove_tr_t1865, sovereign_legitimacy__republican_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(sove_tr_t1920, sovereign_legitimacy__republican_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(sove_tr_t1965, sovereign_legitimacy__republican_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.55).
narrative_ontology:measurement(sove_be_t1865, sovereign_legitimacy__republican_reading, base_extractiveness, 1865, 0.5).
narrative_ontology:measurement(sove_be_t1920, sovereign_legitimacy__republican_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(sove_be_t1965, sovereign_legitimacy__republican_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.7).
narrative_ontology:measurement(sove_su_t1865, sovereign_legitimacy__republican_reading, suppression_requirement, 1865, 0.55).
narrative_ontology:measurement(sove_su_t1920, sovereign_legitimacy__republican_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(sove_su_t1965, sovereign_legitimacy__republican_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sovereign_legitimacy' kernel. It focuses on the upward flow of authority from the people through consent, contrasting with monarchical and constitutional hybrid readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
