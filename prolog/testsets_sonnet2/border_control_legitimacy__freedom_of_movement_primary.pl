% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure Regime, Read Against a Freedom-of-Movement Baseline
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the freedom-of-movement-primary reading of the
 *   border_control_legitimacy kernel: it holds that freedom of movement is a
 *   fundamental human right and that territorial sovereignty, properly
 *   understood, grounds jurisdictional regulation of who is present but does
 *   not ground a categorical authority to exclude. Assessed by this reading's
 *   own lights, the standing arrangement — the closure regime as it actually
 *   operates, with walls, interdiction, detention, and externalized
 *   processing — is the constraint under contest, and it reads as
 *   substantially extractive: displaced persons bear the enforcement cost of
 *   a right the reading holds they are entitled to exercise. This is not a
 *   story about the endorsed alternative (open movement); ε is authored for
 *   the arrangement now in force, exactly as the kernel-reading rule
 *   requires.
 *
 * KEY AGENTS:
 *   - displaced_persons: primary target (powerless/trapped) — bears the closure's enforcement cost
 *   - undocumented_workers: target (powerless/trapped) — bears criminalization cost of exercising the claimed right
 *   - receiving_state_governments: agenda-setter (institutional/arbitrage) — sets and enforces the closure regime
 *   - border_enforcement_industry: concentrated beneficiary (institutional/arbitrage) — profits from apparatus scale
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents the gap between claimed right and operational practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.81).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.88).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure Regime, Read Against a Freedom-of-Movement Baseline").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '8441d298-050f-452b-bea6-56bcd1b39448').
narrative_ontology:cs_kernel_codification('8441d298-050f-452b-bea6-56bcd1b39448', distributed).
narrative_ontology:cs_authority_grounding('8441d298-050f-452b-bea6-56bcd1b39448', distributed).
narrative_ontology:cs_reading_relation('8441d298-050f-452b-bea6-56bcd1b39448', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('8441d298-050f-452b-bea6-56bcd1b39448', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('8441d298-050f-452b-bea6-56bcd1b39448', foundational, movement_right_precedes_territorial_discretion).
narrative_ontology:cs_axiom_status(movement_right_precedes_territorial_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8441d298-050f-452b-bea6-56bcd1b39448', movement_right_precedes_territorial_discretion, deontological).
narrative_ontology:cs_axiom('8441d298-050f-452b-bea6-56bcd1b39448', secondary, sovereignty_limited_to_post_entry_jurisdiction).
narrative_ontology:cs_axiom_status(sovereignty_limited_to_post_entry_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('8441d298-050f-452b-bea6-56bcd1b39448', sovereignty_limited_to_post_entry_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('8441d298-050f-452b-bea6-56bcd1b39448', universal_declaration_freedom_of_movement_baseline).
narrative_ontology:cs_drift_state('8441d298-050f-452b-bea6-56bcd1b39448', post_2015_migration_crisis_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('8441d298-050f-452b-bea6-56bcd1b39448', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_governments).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, undocumented_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, transnational_families).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, westphalian_statehood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fleeing conflict, climate disruption, or economic collapse, they find the territorial line itself — not any individualized assessment of their claim — operating as the primary barrier to safety or livelihood. Under this reading their movement is the exercise of a right; the wall, fence, or maritime interdiction that stops them is what needs justifying, not their crossing. They have no lawful channel commensurate with the scale of need and face detention, refoulement, or death in transit as the enforcement cost of the closure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons, payer,
    powerless, biographical, trapped, global).

% Having crossed despite the closure regime, they work under constant threat of discovery and removal, which suppresses wages, blocks labor organizing, and forecloses recourse against exploitation. From this reading's premise, their presence is not the violation — the criminalization of their movement is — yet they carry the entire enforcement cost as a lived condition of precarity.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, undocumented_workers, payer,
    powerless, biographical, trapped, national).

% Formally entitled to seek protection under international instruments, they are nonetheless subjected to externalized processing, safe-third-country deflection, and indefinite detention that function as de facto closure mechanisms. The gap between the declared right to seek asylum and the operational reality of border denial is where this reading locates the constraint's core illegitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Split across the border by visa regimes, quota systems, and closure enforcement, they bear the closure's cost as prolonged separation, remittance dependency, and the permanent risk that a family member's crossing attempt ends in detention or death. Reunification pathways are narrow, slow, and revocable at state discretion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transnational_families, payer,
    powerless, generational, constrained, global).

% Domestic workers and unions in sectors exposed to migrant labor competition benefit from the closure regime's suppression of labor supply, which supports wage floors and bargaining leverage in specific sectors. They are politically organized and can exit the debate by relocating economic activity or shifting sectors; their benefit is real but concentrated and smaller than the aggregate harm distributed across the excluded population.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_labor_incumbents, beneficiary,
    organized, biographical, mobile, national).

% Private contractors, surveillance technology vendors, and detention operators derive direct revenue from the scale and permanence of the closure apparatus. They lobby to expand enforcement budgets and have no structural incentive to see the underlying displacement drivers resolved, since resolution would shrink their market.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Set and enforce the closure regime through legislation, physical infrastructure, and international deterrence agreements, justifying it as sovereign prerogative and public-order necessity. Under this reading, the government's authority does not extend to categorical exclusion — only to regulating status and rights of those already present — so the government's own foundational claim to closure authority is the thing under contest.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Countries of origin bear the destabilizing effects of blocked emigration — foreclosed remittance economies, pressure-cooker unemployment, and diplomatic subordination in negotiating labor migration terms — but have little seat at the table where receiving states set closure policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% UN treaty bodies, regional human rights courts, and refugee agencies monitor closure practices against instruments like the Universal Declaration and the Refugee Convention, issue findings of noncompliance, and document harms — but possess no enforcement power to compel receiving states to open borders or dismantle interdiction infrastructure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The closure apparatus does perform a real coordination function under a narrower description than 'exclusion': it enables the sequencing of admission processing, screens for genuine security threats, and lets receiving states plan infrastructure and social-service capacity against known population flows.
% TRANSFER_FUNCTION: The arrangement transfers the costs of global displacement — physical risk, wage suppression, family separation, indefinite legal limbo — from receiving-state populations and governments onto displaced persons themselves, while concentrating the narrow economic benefit of reduced labor competition among domestic incumbents and the enforcement-contracting industry.
% ABSENT_VOICES: Displaced persons and sending-state governments are structurally absent from the fora that set closure policy; asylum seekers are nominally present in adjudication processes but those processes are themselves designed and staffed by the excluding state. Under this reading their absence is not incidental — it is the mechanism by which the closure regime avoids confronting the freedom-of-movement claim on its merits.
% DISAPPEARANCE_RATIONALE: If border closure authority were withdrawn overnight and enforcement infrastructure dismantled, global labor markets, remittance flows, urban settlement patterns, and receiving-state welfare-system design would all reorganize substantially within a generation. Enforcement contractors would lose their primary market; displaced populations would move toward opportunity and safety at a scale current systems are built to prevent, which is precisely why this reading treats the current arrangement as load-bearing rather than incidental.
% FOUNDING_PROBLEM: The modern closure regime traces its justification to Westphalian territorial sovereignty and, later, 20th-century state-building projects that used border control to consolidate national labor markets, welfare systems, and security perimeters against external population pressure.
% FOUNDING_PROBLEM_CORROBORATION: Receiving-state governments and enforcement contractors attest the founding problem (security, orderly admission, resource planning) remains live. Independent corroboration from outside the benefiting parties is mixed: UN Special Rapporteurs on the human rights of migrants and multiple regional human rights courts have found that closure enforcement routinely exceeds any security or planning rationale and functions as categorical exclusion inconsistent with treaty obligations — but these bodies lack enforcement power, so their corroboration carries normative rather than operational weight.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because, under this reading, the entire enforcement apparatus extracts safety, mobility, and economic opportunity from people the reading holds have a standing right to move, in service of a sovereignty claim the reading does not recognize as extending to exclusion. Suppression is authored even higher (0.88) because the regime's persistence depends on physical infrastructure, detention, and interdiction that leave displaced persons very little realistic alternative once inside the regime's reach. Theater ratio is authored moderate (0.42) and rising: an increasing share of enforcement activity (biometric surveillance expansion, deterrence messaging, externalized 'processing' centers) functions as performative demonstration of control rather than as the security or planning function that could justify jurisdictional regulation on this reading's own narrower terms.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (receiving_state_governments), the arrangement presents as legitimate sovereign prerogative backed by public consent and security necessity. From the payer seats (displaced_persons, undocumented_workers, asylum_seekers), the same structure operates as coercive extraction of a right the reading holds is theirs by default. The engine will compute these as structurally different seat-classifications from the same authored data — that divergence is the point of the kernel-reading exercise, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced persons, undocumented workers, asylum seekers, and transnational families are declared victims because the closure regime's costs land disproportionately and directly on them, with essentially no exit (trapped/constrained). Receiving-state labor incumbents and the border enforcement industry are declared beneficiaries because they capture concentrated, identifiable value (wage protection, contract revenue) from the regime's continuation. Receiving-state governments are the agenda-setter: they administer and could in principle change the regime, but under this reading their authority to do so is exactly what is under contest, not conceded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly admission, security screening, resource planning) is contested rather than flatly dead: it retains some live content, which is why this reading does not classify the regime as a piton. But the six-questions interview shows corroboration for the 'live problem' claim comes almost entirely from the benefiting parties (receiving-state governments, enforcement contractors), while independent human-rights bodies find the operational scale of exclusion routinely exceeds any security or planning rationale — the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is exactly the signal the R5 genealogy interview is designed to surface, distinguishing genuine residual coordination need from an apparatus that has outgrown its stated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_scope_disagreement,
    'Does territorial sovereignty, as a matter of political-philosophical and international-legal fact, include the authority to categorically exclude non-citizens, or is it limited to jurisdictional regulation of those already present?',
    'This is the core kernel contest and is not resolvable by data internal to this story; it depends on which normative theory of sovereignty (Westphalian/statist vs. cosmopolitan/rights-based) is adopted. The three sibling readings (sovereignty_primary, jurisdictional_sovereignty, freedom_of_movement_primary) each resolve it differently and are authored as separate constraints per the ε-invariance principle.',
    'If sovereignty_primary is correct, this story''s entire victim/beneficiary structure inverts: the closure regime becomes a legitimate exercise of constitutive state authority rather than an extractive suppression of a right. If freedom_of_movement_primary is correct (as authored here), the closure regime is substantially delegitimized regardless of its coordination benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_scope_disagreement, conceptual, 'The unresolved normative disagreement at the root of the kernel, distinguishing this reading from its siblings.').

omega_variable(
    coordination_extraction_separability_under_reading,
    'Even granting freedom-of-movement primacy, is the residual coordination function (security screening, resource planning) separable from the exclusionary apparatus, or does any operational border regime necessarily reproduce exclusion?',
    'Comparative study of high-freedom-of-movement regimes (e.g., EU Schengen internal borders, ECOWAS protocol) that retain identity/security screening without categorical exclusion authority, assessing whether displacement and labor-market disruption analogous to hard-closure regimes still occur.',
    'If separable, this reading would locate essentially all of the measured extraction in the exclusionary component, sharpening the case that ε for the standing arrangement is nearly pure extraction. If inseparable, some baseline extraction would need to be treated as unavoidable coordination cost even under this reading''s own premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_under_reading, conceptual, 'Whether this reading''s own admitted coordination function can exist without reproducing exclusionary extraction.').

omega_variable(
    enforcement_theater_growth_driver,
    'Is the rising theater_ratio (0.20 to 0.42 over 1990-2024) driven by genuine security escalation (more sophisticated threats requiring visible deterrence) or by self-perpetuating budgetary and political incentives within the enforcement industry independent of threat level?',
    'Compare enforcement-budget growth rates against independently measured threat indicators (verified security incidents attributable to unauthorized crossings) over the same interval; divergence would support the self-perpetuation hypothesis.',
    'If self-perpetuating, this supports treating border_enforcement_industry as a rent-seeking beneficiary whose interest in the regime''s continuation is decoupled from the founding problem, strengthening the mandatrophy reading (T17-relevant: rising extraction over time on a claim with contested naturalness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_theater_growth_driver, empirical, 'Whether escalating enforcement theater tracks genuine threat or self-perpetuating institutional incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t1996, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1996, 0.24).
narrative_ontology:measurement(bord_tr_t2002, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(bord_tr_t2008, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2008, 0.34).
narrative_ontology:measurement(bord_tr_t2014, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(bord_tr_t2020, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(bord_be_t1996, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1996, 0.63).
narrative_ontology:measurement(bord_be_t2002, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2002, 0.69).
narrative_ontology:measurement(bord_be_t2008, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement(bord_be_t2014, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2014, 0.77).
narrative_ontology:measurement(bord_be_t2020, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(bord_su_t1996, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1996, 0.62).
narrative_ontology:measurement(bord_su_t2002, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2002, 0.71).
narrative_ontology:measurement(bord_su_t2008, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2008, 0.78).
narrative_ontology:measurement(bord_su_t2014, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2014, 0.83).
narrative_ontology:measurement(bord_su_t2020, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2020, 0.86).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'border control legitimacy,' per the ε-invariance principle. sovereignty_primary treats the same standing arrangement as legitimate constitutive state authority with negligible extraction; jurisdictional_sovereignty treats it as legitimate only within a balancing test, with moderate, contested extraction; freedom_of_movement_primary (this file) treats it as substantially extractive because the reading denies the closure authority's premise entirely. The three stories share no beneficiary/victim structure and must not be merged or averaged — they are linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
