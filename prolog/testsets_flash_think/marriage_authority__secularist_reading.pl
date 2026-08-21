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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Marriage Authority: Secularist Reading (Uniform Civil Code)
 *   domain: legal/political/social/comparative_family_law
 *
 * SUMMARY:
 *   This constraint represents the 'secularist reading' of marriage
 *   authority, which posits that family law should be uniform and legislated
 *   by the democratic state, with personal law pluralism seen as a temporary
 *   anomaly to be eliminated via a Uniform Civil Code (UCC). This reading is
 *   a zero-sum challenge to communal legal autonomy, characterized by high
 *   extraction from minority religious communities and high suppression of
 *   alternative legal systems. The claimed type is 'tangled_rope' because it
 *   presents a coordination function (national unity, equality) as cover for
 *   asymmetric extraction (loss of communal legal autonomy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.85).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.9).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Marriage Authority: Secularist Reading (Uniform Civil Code)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/political/social/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac').
narrative_ontology:cs_kernel_codification('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', formalized).
narrative_ontology:cs_authority_grounding('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', lineage).
narrative_ontology:cs_interpretation_layer_present('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac').
narrative_ontology:cs_reading_relation('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', foundational, legislative_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', legislative_supremacy_in_family_law, conventional).
narrative_ontology:cs_axiom('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', foundational, uniformity_as_modernity_and_equality).
narrative_ontology:cs_axiom_status(uniformity_as_modernity_and_equality, holdable).
narrative_ontology:cs_axiom_grounding('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', uniformity_as_modernity_and_equality, instrumental).
narrative_ontology:cs_reference_frame('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', secular_democratic_state_uniform_law).
narrative_ontology:cs_drift_state('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', contemporary_pluralism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc1c33c9-5bf9-4ebc-ad0f-5a6ea3881aac', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, democratic_legislature).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, traditional_religious_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a Uniform Civil Code (UCC) to replace diverse personal laws, viewing it as essential for national unity, gender equality, and modern governance. Benefits from the expansion of state authority and the reduction of communal influence.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary).

% Currently governed by their respective personal laws, which they see as integral to their religious and cultural identity. They would bear the direct cost of losing legal autonomy and having state-defined norms imposed on their family life. Exit is identity-locked as their self-conception is tied to their communal legal traditions.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, minority_religious_communities, excluded).

% Possesses the constitutional authority to enact a Uniform Civil Code. From this reading's perspective, it is the legitimate body to define marriage and family law for all citizens, overcoming historical pluralism. Benefits from consolidating legal authority and projecting a unified national identity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, biographical, mobile, national).

% Often align with the secularist reading's goal of achieving gender equality, particularly where personal laws are seen as discriminatory against women. They benefit from the potential for a UCC to enshrine equal rights, though they may contest specific provisions of any proposed code.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, biographical, constrained, national).

% Act as custodians and interpreters of personal laws within their communities. They would lose significant authority and influence if a UCC were enacted, as their role in adjudicating family matters would be superseded by state law. Their position is identity-locked to the preservation of communal legal autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, traditional_religious_leaders, payer,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, traditional_religious_leaders, excluded).

% Would be tasked with interpreting and upholding the constitutionality of a UCC, potentially mediating between legislative intent and fundamental rights claims. They observe the political contest but ultimately enforce the enacted law.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a single, uniform legal framework for marriage, divorce, inheritance, and adoption across all citizens, eliminating legal pluralism and ensuring equal application of law regardless of religious affiliation.
% TRANSFER_FUNCTION: Transfers legal authority over family matters from diverse religious and communal institutions to the democratic legislature and state courts. It also transfers the burden of adapting to a new legal system onto minority religious communities.
% ABSENT_VOICES: Those who advocate for the preservation of personal law pluralism as a fundamental right or as a feature of federalism are actively marginalized in the secularist discourse, often framed as anti-modern or anti-national. Their arguments for communal autonomy are excluded from the dominant legislative agenda.
% DISAPPEARANCE_RATIONALE: If the secularist reading's push for a UCC vanished, the existing system of personal law pluralism would persist, and the ongoing political and legal contestation around family law would continue in its current fragmented form. The state's role in family law would remain limited, and communal institutions would retain their authority.
% FOUNDING_PROBLEM: The perceived fragmentation, inequality, and lack of national unity arising from diverse personal laws governing family matters, which are seen as an anomaly in a modern, secular democratic state.
% FOUNDING_PROBLEM_CORROBORATION: Secular intellectuals, some women's rights organizations, and proponents of national integration attest that the problems of fragmentation and inequality persist. However, minority community leaders and some legal scholars dispute the framing, arguing that pluralism is a strength or a protected right, not a problem to be solved by uniformity.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the implementation of a UCC would fundamentally alter the legal landscape for minority communities, imposing state norms and extracting their traditional legal autonomy. Suppression is very high (0.90) as it requires active legislative and enforcement power to overcome significant resistance and dismantle existing personal law systems. The theater ratio is low (0.10) because the project is a direct, active political and legal endeavor, not primarily performative. Accessibility collapse is high (0.80) as the goal is to eliminate alternative legal frameworks. Resistance is high (0.75) due to strong opposition from affected communities.
 *
 * PERSPECTIVAL GAP:
 *   From the secularist perspective, the UCC is a necessary step towards a modern, equitable nation-state. From the perspective of minority religious communities, it is an imposition that erodes their identity and autonomy. The engine's classification will highlight this divergence by computing high extraction for the victim seats, contrasting with the claimed 'rope' (coordination) function.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'secular_modernist_coalition' and the 'democratic_legislature' are the primary beneficiaries and agenda-setters, gaining legal authority, national uniformity, and ideological validation. 'Minority_religious_communities' and 'traditional_religious_leaders' are the primary targets/victims, losing legal autonomy and cultural distinctiveness. 'Gender_equality_advocates' are beneficiaries to the extent that a UCC advances their goals, though their alignment may be conditional on the specific content of the code.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the UCC as a pure 'rope' (coordination) by highlighting the substantial extraction and suppression involved. While proponents frame it as solving a 'founding problem' of national fragmentation and inequality, the high extractiveness and resistance indicate it is not a universally beneficial coordination mechanism but rather a contested project with clear winners and losers. The 'tangled_rope' classification captures this hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pluralism_as_anomaly_or_right,
    'Is personal law pluralism a transitional anomaly to be eliminated for national unity and equality, or a fundamental right to communal autonomy and a feature of a diverse federal system?',
    'Constitutional amendment explicitly defining the scope of religious freedom and communal legal rights, or a national referendum on the value of legal pluralism versus uniformity.',
    'If pluralism is affirmed as a right, the secularist reading''s extractiveness would be reclassified as illegitimate, potentially shifting its type towards a ''snare''. If uniformity is affirmed, the resistance from minority communities would be reclassified as illegitimate opposition to a ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralism_as_anomaly_or_right, conceptual, 'The fundamental conceptual disagreement over the nature and legitimacy of legal pluralism in family law.').

omega_variable(
    gender_equality_outcome_of_ucc,
    'Would a Uniform Civil Code genuinely advance gender equality for all women, or would it primarily impose a majoritarian secular norm that may not address specific intra-community gender inequalities?',
    'Empirical studies comparing gender equality outcomes in jurisdictions with and without UCCs, and detailed analysis of proposed UCC provisions against existing personal laws.',
    'If a UCC fails to significantly improve gender equality or creates new forms of inequality, the ''coordination'' justification of the secularist reading would be weakened, increasing its effective extraction and potentially shifting its type towards a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_outcome_of_ucc, empirical, 'The empirical question of whether a UCC achieves its stated goal of gender equality.').

omega_variable(
    resistance_mechanism_ambiguity,
    'Is the resistance to a Uniform Civil Code primarily driven by genuine religious conviction and cultural preservation, or by the vested interests of traditional religious leaders seeking to maintain their authority?',
    'Sociological surveys of community members'' attitudes towards personal law reform, independent of religious leadership, and analysis of the financial and social capital of traditional leaders.',
    'If resistance is primarily driven by elite capture, the ''identity_locked'' exit option for minority communities might be re-evaluated as ''constrained'' (due to internal power dynamics), and the suppression metric might be seen as more effective against a less unified opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_mechanism_ambiguity, empirical, 'Understanding the true drivers of resistance to the Uniform Civil Code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority__secularist_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(marr_tr_t1965, marriage_authority__secularist_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__secularist_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__secularist_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__secularist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority__secularist_reading, base_extractiveness, 1947, 0.6).
narrative_ontology:measurement(marr_be_t1965, marriage_authority__secularist_reading, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__secularist_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__secularist_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__secularist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority__secularist_reading, suppression_requirement, 1947, 0.65).
narrative_ontology:measurement(marr_su_t1965, marriage_authority__secularist_reading, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__secularist_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__secularist_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__secularist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'marriage_authority' kernel. Each reading presents a distinct structural claim about the source and scope of marriage law, leading to different ε values and classifications. This secularist reading directly challenges the communal and federalist readings, while influencing and partially aligning with the gender rights and judicial harmonization readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
