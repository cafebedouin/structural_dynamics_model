% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading of Marriage Authority: Uniform Civil Code
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   This constraint represents the 'secularist reading' of marriage
 *   authority, where the democratic legislature is the sole legitimate source
 *   of family law, and personal law pluralism is a temporary anomaly to be
 *   eliminated by a Uniform Civil Code (UCC). It is framed as a tangled rope
 *   because it genuinely aims to coordinate a unified legal system
 *   (beneficiaries: secular-modernist coalition, gender equality advocates)
 *   but does so through asymmetric extraction from minority religious
 *   communities, whose traditional laws are targeted for elimination
 *   (victims: minority religious communities, personal law boards). The
 *   constraint requires active enforcement to overcome resistance to the UCC.
 *
 * KEY AGENTS:
 *   - state_legislature: Agenda-setter (institutional/mobile) — drives the UCC agenda
 *   - secular_modernist_coalition: Beneficiary (organized/mobile) — advocates for and benefits from a unified secular code
 *   - minority_religious_communities: Payer (powerless/identity_locked) — bears the cost of losing traditional laws
 *   - religious_personal_law_boards: Payer (moderate/constrained) — loses authority under UCC
 *   - gender_equality_advocates: Beneficiary (organized/mobile) — supports UCC for universal rights
 *   - federalist_proponents: Excluded (organized/constrained) — arguments for pluralism are sidelined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.7).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.65).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading of Marriage Authority: Uniform Civil Code").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '8e7246e6-4a7e-411c-90fb-ac249a3008e3').
narrative_ontology:cs_kernel_codification('8e7246e6-4a7e-411c-90fb-ac249a3008e3', formalized).
narrative_ontology:cs_authority_grounding('8e7246e6-4a7e-411c-90fb-ac249a3008e3', lineage).
narrative_ontology:cs_interpretation_layer_present('8e7246e6-4a7e-411c-90fb-ac249a3008e3').
narrative_ontology:cs_reading_relation('8e7246e6-4a7e-411c-90fb-ac249a3008e3', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('8e7246e6-4a7e-411c-90fb-ac249a3008e3', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('8e7246e6-4a7e-411c-90fb-ac249a3008e3', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('8e7246e6-4a7e-411c-90fb-ac249a3008e3', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('8e7246e6-4a7e-411c-90fb-ac249a3008e3', foundational, legislative_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('8e7246e6-4a7e-411c-90fb-ac249a3008e3', legislative_supremacy_in_family_law, conventional).
narrative_ontology:cs_axiom('8e7246e6-4a7e-411c-90fb-ac249a3008e3', foundational, legal_pluralism_as_transitional_anomaly).
narrative_ontology:cs_axiom_status(legal_pluralism_as_transitional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('8e7246e6-4a7e-411c-90fb-ac249a3008e3', legal_pluralism_as_transitional_anomaly, instrumental).
narrative_ontology:cs_reference_frame('8e7246e6-4a7e-411c-90fb-ac249a3008e3', secular_unified_legal_system).
narrative_ontology:cs_drift_state('8e7246e6-4a7e-411c-90fb-ac249a3008e3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e7246e6-4a7e-411c-90fb-ac249a3008e3', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, state_legislature).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, religious_personal_law_boards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts its exclusive right to legislate on marriage and family matters, aiming to replace diverse personal laws with a single Uniform Civil Code. Benefits from consolidating legal authority and promoting a unified national identity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Advocates for the Uniform Civil Code as a means to achieve gender equality, national integration, and secular governance. Benefits from the perceived progress and modernization associated with a unified legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of losing their traditional, religiously-derived personal laws, which they view as integral to their cultural and religious identity. They face the prospect of state-imposed norms replacing community-based practices, with limited avenues for resistance.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    powerless, generational, identity_locked, national).

% Currently administer personal laws for their respective communities. They would lose their authority and institutional relevance under a Uniform Civil Code, representing a direct challenge to their power and traditional role.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_personal_law_boards, payer,
    moderate, biographical, constrained, national).

% Support the Uniform Civil Code as a necessary step to eliminate discriminatory practices within existing personal laws, particularly those affecting women. They see the secularist reading as a pathway to universal rights.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, generational, mobile, national).

% Would argue against the centralization of marriage authority, advocating for a federal structure that respects regional and community diversity. Their concerns about majoritarianism are sidelined by the secularist push for uniformity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, federalist_proponents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse family laws into a single, unified legal framework, simplifying legal administration and promoting a common national identity.
% TRANSFER_FUNCTION: Transfers authority over marriage and family law from diverse religious communities and their traditional institutions to the central democratic legislature, along with the power to define and enforce these norms.
% ABSENT_VOICES: Proponents of communal autonomy and federalist principles are largely excluded from the dominant discourse, which frames legal pluralism as an anomaly rather than a legitimate form of governance. Their arguments for preserving community-specific laws or decentralized authority are dismissed.
% DISAPPEARANCE_RATIONALE: If the secularist reading and its push for a UCC vanished, the existing system of personal laws would persist, and the political landscape would shift to accommodate ongoing debates about legal pluralism, potentially empowering communal and federalist readings. The state's legislative agenda would be significantly altered.
% FOUNDING_PROBLEM: The perceived fragmentation and inequality arising from multiple personal laws, particularly concerning women's rights and national integration, which were seen as obstacles to a modern, secular state.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by the secularist-modernist coalition and gender equality advocates, who point to ongoing disparities and the need for national unity. Minority religious communities and federalist proponents contest this, arguing that the problem is overstated or that the proposed solution (UCC) creates new problems of cultural erosion and majoritarian imposition. International human rights bodies also offer corroboration on gender inequality issues within some personal laws.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the UCC, from this reading, demands the surrender of community-specific legal autonomy, which is a significant cost for affected groups. Suppression (0.65) is substantial as the state actively works to delegitimize and eventually replace personal laws, often against strong community resistance. Theater ratio is low (0.2) as the legislative and political efforts are genuinely aimed at implementing the UCC, not merely performing. Resistance is high (0.75) due to strong opposition from affected communities.
 *
 * PERSPECTIVAL GAP:
 *   The state legislature and secular-modernist coalition perceive this as a necessary coordination effort for national unity and equality, leading to a 'rope-like' experience. In contrast, minority religious communities experience it as a 'snare' due to the forced assimilation and loss of identity-linked legal frameworks. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legislature and secular-modernist coalition are clear beneficiaries (d near 0.0) as they gain consolidated authority and a unified legal system. Minority religious communities and personal law boards are targets (d near 1.0) as they face the loss of their legal traditions and institutional power. Gender equality advocates are beneficiaries, aligning with the secularist goal of universal rights. Federalist proponents are excluded, their position structurally undermined by the centralizing thrust.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively pursuing its mandate (UCC implementation), so mandatrophy is not resolved. However, the 'contested' status of the founding problem highlights a potential future mandatrophy: if the problem of inequality can be addressed through other means (e.g., judicial harmonization) without eliminating pluralism, the UCC's mandate could become obsolete, but the constraint might persist due to the beneficiaries' interest in consolidated power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secularism_vs_pluralism_ambiguity,
    'Is the elimination of personal law pluralism a necessary consequence of secular governance, or can secularism coexist with and protect legal pluralism?',
    'Comparative legal analysis of other secular states that maintain forms of legal pluralism, or a constitutional amendment explicitly defining the relationship between secularism and community rights.',
    'If secularism can coexist with pluralism, the ''extraction'' component of this constraint (forced uniformity) would be reclassified as unnecessary, potentially shifting the constraint towards a ''snare'' or ''piton'' if maintained for other reasons. If inseparable, the extraction is framed as an unavoidable cost of the secularist project.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secularism_vs_pluralism_ambiguity, conceptual, 'The conceptual boundary between secular governance and legal pluralism.').

omega_variable(
    gender_equality_path_ambiguity,
    'Is a Uniform Civil Code the only effective path to gender equality in family law, or can gender-just reforms be achieved within existing personal law frameworks?',
    'Empirical study of gender equality outcomes in jurisdictions that have reformed personal laws versus those with a UCC, or a judicial ruling mandating gender-just interpretations across all personal laws without legislative uniformity.',
    'If gender equality can be achieved within pluralistic frameworks, the ''gender equality'' justification for the UCC (a key coordination narrative) would weaken, exposing the ''extraction'' from minority communities as less justified. This would amplify the ''snare'' aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_path_ambiguity, empirical, 'The necessity of UCC for achieving gender equality.').

omega_variable(
    identity_lock_strength,
    'How deeply is the identity of minority religious communities fused with their personal laws, and what would be the social cost of their elimination?',
    'Sociological and anthropological studies on community identity formation and the role of personal law, combined with surveys on perceived loss and resistance to a UCC.',
    'A stronger identity lock implies higher effective suppression and extraction, as the cost of exit (loss of identity) is amplified. This would push the constraint further towards a ''snare'' from the perspective of these communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which community identity is tied to personal laws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority__secularist_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__secularist_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__secularist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__secularist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__secularist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority__secularist_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__secularist_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__secularist_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__secularist_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__secularist_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority__secularist_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__secularist_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__secularist_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__secularist_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__secularist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel. It directly challenges the communal autonomy and federalist millet readings, while seeking to achieve gender equality goals through legislative uniformity, contrasting with judicial harmonization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
