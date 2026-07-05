% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Co-Production Standard for Legitimate Knowledge (Hybrid Reading)
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates the hybrid co-production reading of the
 *   legitimate-knowledge-boundary kernel: the claim that knowledge is only
 *   legitimate when methodological rigor AND experiential validity are
 *   integrated through formal co-production processes. This reading emerged
 *   partly as a corrective to both the credentialed-expertise reading
 *   (accused of ignoring lived experience) and the experiential-pluralism
 *   reading (accused of abandoning rigor entirely). But the correction
 *   created its own gatekeeping structure: a class of boundary-spanning
 *   intermediary institutions now administers what counts as properly
 *   integrated knowledge, and access to that administration is itself
 *   unevenly distributed. The genuine coordination function (rigor and
 *   experience really do produce better findings together in many documented
 *   cases) coexists with a real extraction dynamic (intermediary capture,
 *   exclusion of the unbrokered, cost barriers for under-resourced
 *   researchers and disconnected communities).
 *
 * KEY AGENTS:
 *   - coproduction_intermediary_institutions: agenda_setter/beneficiary (institutional/arbitrage) — designs and certifies the standard
 *   - unaffiliated_community_knowledge_holders: payer (powerless/trapped) — excluded from legitimation despite holding relevant knowledge
 *   - early_career_researchers_without_coproduction_access: payer (moderate/constrained) — structurally disadvantaged by infrastructure requirements
 *   - epistemology_of_science_scholars: observer (analytical/analytical) — evaluates whether integration claims hold empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.46).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Co-Production Standard for Legitimate Knowledge (Hybrid Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'f08a8375-3311-4081-a65f-391c23f2a273').
narrative_ontology:cs_kernel_codification('f08a8375-3311-4081-a65f-391c23f2a273', distributed).
narrative_ontology:cs_authority_grounding('f08a8375-3311-4081-a65f-391c23f2a273', practice).
narrative_ontology:cs_interpretation_layer_present('f08a8375-3311-4081-a65f-391c23f2a273').
narrative_ontology:cs_reading_relation('f08a8375-3311-4081-a65f-391c23f2a273', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('f08a8375-3311-4081-a65f-391c23f2a273', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('f08a8375-3311-4081-a65f-391c23f2a273', foundational, integration_is_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(integration_is_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f08a8375-3311-4081-a65f-391c23f2a273', integration_is_necessary_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('f08a8375-3311-4081-a65f-391c23f2a273', secondary, formal_coproduction_process_is_required_not_optional).
narrative_ontology:cs_axiom_status(formal_coproduction_process_is_required_not_optional, holdable).
narrative_ontology:cs_axiom_grounding('f08a8375-3311-4081-a65f-391c23f2a273', formal_coproduction_process_is_required_not_optional, conventional).
narrative_ontology:cs_reference_frame('f08a8375-3311-4081-a65f-391c23f2a273', post_credibility_crisis_integration_mandate).
narrative_ontology:cs_drift_state('f08a8375-3311-4081-a65f-391c23f2a273', contemporary_funder_conditionality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f08a8375-3311-4081-a65f-391c23f2a273', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_intermediary_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organization_staff).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, funders_of_participatory_research).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, early_career_researchers_without_coproduction_access).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, communities_lacking_institutional_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, knowledge_integration_thesis).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_epistemic_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer co-production frameworks — convening protocols, dual-validation checklists, community advisory boards — that determine what counts as legitimately integrated knowledge. Their institutional survival depends on co-production remaining the required standard; they train other institutions in the methodology and certify compliance, capturing grant funding and consulting fees tied to the standard's continued operation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_intermediary_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_intermediary_institutions, beneficiary).

% Occupy the professional niche of 'translators' between methodological researchers and experiential knowledge holders. Their careers exist because the dual-validation requirement creates demand for facilitation labor; without the requirement, researchers and communities could interact directly or not at all, eliminating the intermediary role.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organization_staff, beneficiary,
    organized, biographical, constrained, national).

% Grant-making bodies that require co-production as a condition of funding, allowing them to claim their portfolios are rigorous and inclusive simultaneously. They gain reputational and political cover from mandating the hybrid standard, regardless of whether it improves outcomes for the communities involved.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, funders_of_participatory_research, beneficiary,
    institutional, generational, arbitrage, global).

% Must now budget years and resources for community engagement processes before their methodological findings are accepted as legitimate in co-production-mandated venues. Well-resourced labs absorb this as added prestige; smaller labs experience it as a tax on publication and funding eligibility they cannot easily pay.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers, beneficiary).

% Hold experiential knowledge but lack the institutional relationships to be selected into a formal co-production process. Their knowledge is excluded from 'legitimate' status not because it fails validation but because no boundary organization brokered its inclusion — the requirement for formal integration screens out those without pre-existing institutional access.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders, payer,
    powerless, biographical, trapped, local).

% Junior scholars at under-resourced institutions cannot afford the multi-year community partnership infrastructure the hybrid standard requires. They are structurally disadvantaged relative to peers at well-funded institutions with existing boundary-organization relationships, even when their methodological work is equally rigorous.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, early_career_researchers_without_coproduction_access, payer,
    moderate, biographical, constrained, national).

% Would have relevant experiential knowledge to contribute but have never been approached by an intermediary institution for a co-production partnership. Their absence from the legitimation process is invisible within the standard's own accounting — it only tracks knowledge that has already been co-produced, not knowledge that was never brokered.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, communities_lacking_institutional_partners, excluded,
    powerless, generational, trapped, local).

% Study whether co-production genuinely improves epistemic outcomes (better predictions, more actionable findings) or primarily redistributes gatekeeping authority to a new class of intermediaries while leaving the underlying legitimacy contest over what counts as knowledge unresolved.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemology_of_science_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_intermediary_institutions).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem: methodologically rigorous research has historically ignored or misrepresented the knowledge of affected communities, producing findings that are technically valid but practically useless or harmful when implemented; formal co-production processes create structured channels for combining rigor and lived experience that neither pure expertise nor pure experiential validation reliably produces alone.
% TRANSFER_FUNCTION: Moves gatekeeping authority and resource control from single-track credentialing bodies AND from unmediated communities toward a new class of boundary-spanning intermediary institutions and staff, who capture facilitation fees, consulting contracts, and grant administration roles created by the dual-validation requirement.
% ABSENT_VOICES: Communities and knowledge holders who have no pre-existing relationship with a boundary organization are structurally invisible to the standard — they cannot object to their exclusion because the process that would register their objection is the same process they are excluded from. Under-resourced researchers at teaching-intensive institutions similarly lack a forum to contest a standard that assumes multi-year partnership capacity they do not have.
% DISAPPEARANCE_RATIONALE: Boundary organizations and their funders would say the world rearranges catastrophically — research would revert to purely credentialed gatekeeping or purely experiential claims, losing whatever genuine integration gains exist. Excluded communities and under-resourced researchers would say the world barely changes for them either way, since they were never inside the co-production process to begin with; the standard's disappearance would mainly cost the intermediary class its niche.
% FOUNDING_PROBLEM: Decades of methodologically rigorous research producing findings that communities experienced as extractive, misdirected, or actively harmful (e.g., environmental health studies that missed locally known contamination pathways, clinical research that ignored patient-reported outcomes) created a credibility crisis for pure credentialed-expertise legitimation.
% FOUNDING_PROBLEM_CORROBORATION: Independent program evaluators and some funders outside the boundary-organization ecosystem attest that co-production has improved uptake and relevance in specific well-documented cases (participatory environmental health research, indigenous-led ecological monitoring). But several evaluations commissioned by funders themselves report inconclusive or absent evidence that formal co-production outperforms less bureaucratic community engagement, and no corroboration exists from the excluded communities and researchers who were never surveyed because they were never brought into a co-production process.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) — lower than a pure gatekeeping snare because the coordination function is real and documented in specific cases, but rising over the interval as the co-production standard hardened from an emergent best-practice into a mandatory funding condition, generating a rent-seeking layer (facilitation fees, certification consulting) on top of the genuine integration function. Suppression is moderate (0.42): the standard does not forbid alternative epistemic practices outright, but funding conditionality creates real pressure to conform. Theater ratio rises to 0.38 as more co-production activity becomes procedural compliance (checklists, advisory board minutes) rather than substantive integration — a Goodhart-style drift the temporal series is designed to surface. Accessibility collapse is moderate (0.4): credentialed-only and experiential-only pathways still exist in some venues, so alternatives have not fully collapsed, but funder mandates are narrowing them.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this looks like principled methodology: a considered response to documented failures of single-track legitimation. From the excluded-community seat, it looks like a new credentialing barrier dressed in participatory language — the requirement for FORMAL integration screens out those who have the relevant knowledge but not the institutional relationship needed to be formally integrated. Both readings are structurally accurate from their respective positions; the engine's per-seat computation is expected to diverge along exactly this line.
 *
 * DIRECTIONALITY LOGIC:
 *   Coproduction intermediary institutions and boundary organization staff sit at the beneficiary end: the standard's existence is the source of their institutional and professional relevance, and they administer its enforcement. Funders benefit reputationally without bearing the implementation cost. Credentialed researchers are secondary beneficiaries where well-resourced but genuine payers where under-resourced — this bifurcation is why early_career_researchers_without_coproduction_access is authored as a distinct seat from credentialed_researchers rather than merged. Unaffiliated community knowledge holders and communities lacking institutional partners sit at the full-target end: they bear the cost of exclusion (their knowledge is delegitimized by omission) without ever entering the process that would let them contest it — trapped exit options reflect that there is no accessible route into the legitimation process from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigor-only research producing extractive or misdirected findings) was genuinely live when co-production frameworks emerged, and remains partly live in specific well-documented domains — this prevents a blanket 'pure extraction' reading. But the standard's benefiting parties are largely the ones certifying its continued necessity, and the excluded communities and researchers have no forum to register that the mandate has, for their situation, become gatekeeping rather than remedy. The tangled_rope classification captures this precisely: a real coordination function persists alongside asymmetric extraction that requires active enforcement (funding conditionality) to sustain — collapsing it to either pure rope or pure snare would erase one half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coproduction_epistemic_gain_vs_gatekeeping_redistribution,
    'Does formal co-production genuinely produce epistemically superior knowledge (better predictions, more actionable findings) compared to either pure credentialed research or pure experiential validation, or does it primarily redistribute gatekeeping authority from credentialing bodies to a new intermediary class without a corresponding epistemic gain?',
    'Comparative outcome studies across matched cases — same research question pursued via credentialed-only, experiential-only, and co-production pathways, evaluated by outcome measures not chosen by any of the three legitimation communities themselves.',
    'If co-production shows a genuine, reproducible epistemic advantage, the coordination function dominates and the tangled_rope reading tilts toward rope with a smaller extractive residue. If the outcome advantage is not reproducible outside cases hand-selected by boundary organizations, this reading is closer to a snare wearing coordination language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coproduction_epistemic_gain_vs_gatekeeping_redistribution, empirical, 'Whether co-production''s integration claim is empirically vindicated or primarily a redistribution of legitimation authority.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to model ''legitimate knowledge'' via three parallel readings (credentialed, experiential, hybrid) itself neutral, or does treating hybrid co-production as a distinct third reading already presuppose that integration is possible/desirable — a premise the other two readings would each deny from their own foundations?',
    'This is a conceptual/framing question rather than an empirical one: examine whether credentialed_expertise_reading and experiential_pluralism_reading proponents would recognize hybrid_coproduction_reading as a genuine third position or would each claim it collapses into their own reading under scrutiny (rigor advocates might say ''co-production is just rigorous research with a consultation step''; experiential advocates might say ''co-production is just community validation with extra paperwork'').',
    'If the hybrid reading is not recognized as structurally distinct by either sibling''s own adherents, its coordination claim is weaker than authored here, and more of its measured extraction should be read as a legitimation contest between the other two readings displaced onto a third label.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the hybrid reading is a genuine third position or a relabeling of the credentialed/experiential contest.').

omega_variable(
    unbrokered_knowledge_invisibility,
    'How much relevant experiential knowledge exists that has never been captured by any co-production process, and is therefore invisible to any assessment of the standard''s coverage or fairness?',
    'Field surveys attempting to independently identify communities with relevant knowledge who have never been approached by a boundary organization, compared against the population of communities that HAVE been engaged, to estimate the scale of the exclusion.',
    'A large unbrokered population would substantially raise the effective suppression/accessibility_collapse figures for the excluded seats, since the standard''s own self-assessment cannot detect this population by construction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unbrokered_knowledge_invisibility, empirical, 'The scale of experiential knowledge excluded from co-production because it was never brokered into the process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(legi_su_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'legitimate knowledge boundary' per the ε-invariance principle. credentialed_expertise_reading and experiential_pluralism_reading are separate files with their own ε, beneficiaries, and classification. This hybrid reading claims a moderate, rising ε (0.46) driven by intermediary-capture dynamics distinct from either sibling's extraction mechanism. All three should link to each other bidirectionally as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
