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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading: Legislative Supremacy in Marriage Law
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   This constraint represents the 'secularist reading' of marriage
 *   authority, where the democratic legislature is the sole legitimate source
 *   of family law, and personal law pluralism is a temporary anomaly to be
 *   eliminated by a Uniform Civil Code (UCC). It is a zero-sum challenge to
 *   communal autonomy, with the secular-modernist coalition and state
 *   legislature as beneficiaries, and minority religious communities as
 *   victims. The constraint is classified as a Tangled Rope due to its
 *   genuine coordination function (legal uniformity) coupled with high
 *   asymmetric extraction and active enforcement against dissenting
 *   communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.78).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.85).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading: Legislative Supremacy in Marriage Law").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/political/social").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'dafd681b-b4a3-42d7-95e3-a9fab9001581').
narrative_ontology:cs_kernel_codification('dafd681b-b4a3-42d7-95e3-a9fab9001581', formalized).
narrative_ontology:cs_authority_grounding('dafd681b-b4a3-42d7-95e3-a9fab9001581', lineage).
narrative_ontology:cs_interpretation_layer_present('dafd681b-b4a3-42d7-95e3-a9fab9001581').
narrative_ontology:cs_reading_relation('dafd681b-b4a3-42d7-95e3-a9fab9001581', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('dafd681b-b4a3-42d7-95e3-a9fab9001581', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('dafd681b-b4a3-42d7-95e3-a9fab9001581', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('dafd681b-b4a3-42d7-95e3-a9fab9001581', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('dafd681b-b4a3-42d7-95e3-a9fab9001581', foundational, legislative_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('dafd681b-b4a3-42d7-95e3-a9fab9001581', legislative_supremacy_in_family_law, conventional).
narrative_ontology:cs_axiom('dafd681b-b4a3-42d7-95e3-a9fab9001581', foundational, uniformity_as_national_integration).
narrative_ontology:cs_axiom_status(uniformity_as_national_integration, holdable).
narrative_ontology:cs_axiom_grounding('dafd681b-b4a3-42d7-95e3-a9fab9001581', uniformity_as_national_integration, instrumental).
narrative_ontology:cs_reference_frame('dafd681b-b4a3-42d7-95e3-a9fab9001581', secular_democratic_legislative_authority).
narrative_ontology:cs_drift_state('dafd681b-b4a3-42d7-95e3-a9fab9001581', contemporary_pluralist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dafd681b-b4a3-42d7-95e3-a9fab9001581', '').
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

% Advocates for a Uniform Civil Code (UCC) to eliminate religious personal laws, viewing it as essential for national unity, gender equality, and secular governance. Benefits from the expansion of state legislative authority over family matters.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, mobile, national).

% Holds the constitutional mandate to legislate on family law. Seeks to consolidate its authority by enacting a UCC, thereby standardizing marriage and family norms across all communities. Benefits from increased control and reduced legal fragmentation.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_legislature, agenda_setter,
    institutional, biographical, constrained, national).

% Subject to existing personal laws derived from their religious traditions. They view the imposition of a UCC as an infringement on their religious freedom and cultural autonomy, forcing them to abandon deeply held identity-forming practices. Their exit is identity-locked, as their self-concept is tied to their communal legal traditions.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    powerless, generational, identity_locked, national).

% Administer personal laws for their respective communities. They face the loss of their institutional authority and social function if a UCC is enacted, as their role in adjudicating marriage and family disputes would be superseded by state law.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_personal_law_boards, payer,
    moderate, generational, constrained, national).

% Support a UCC as a means to achieve gender equality, arguing that many personal laws contain discriminatory provisions against women. They benefit from the potential for standardized, non-discriminatory family law.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, generational, mobile, national).

% Interprets the constitutionality of personal laws and potential UCC legislation. Its rulings can either affirm legislative supremacy, protect communal autonomy, or prioritize gender equality, shaping the trajectory of marriage authority.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse personal laws into a single, uniform civil code, thereby simplifying legal administration, promoting national integration, and ensuring legal equality for all citizens regardless of religious affiliation.
% TRANSFER_FUNCTION: Transfers authority over marriage and family law from religious communities and their traditional institutions to the secular state legislature, along with the associated social and political capital.
% ABSENT_VOICES: Traditionalists within minority religious communities who are not represented by official boards, and who would argue for the preservation of their distinct cultural and religious identities against state homogenization, are often marginalized in the public discourse.
% DISAPPEARANCE_RATIONALE: If the secularist drive for legislative supremacy and a UCC vanished, the existing system of personal law pluralism would persist indefinitely, communal institutions would retain their authority, and the political landscape around family law would remain fragmented, requiring a different coordination mechanism.
% FOUNDING_PROBLEM: The perceived fragmentation of national identity and legal inequality stemming from diverse religious personal laws, seen as an impediment to modern nation-building and secular governance.
% FOUNDING_PROBLEM_CORROBORATION: The secularist-modernist coalition and many gender equality advocates attest that the problem of legal fragmentation and inequality is still live and urgent. Minority religious communities and federalist scholars contest this, arguing that pluralism is a strength or a necessary protection, not a problem.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the imposition of a UCC would force minority communities to abandon deeply ingrained religious and cultural practices, representing a significant cost to their identity and autonomy. Suppression is also high (0.85) as the state actively enforces its legislative supremacy, using legal and political mechanisms to marginalize and eventually eliminate personal law systems. The theater ratio is low (0.15) because the push for a UCC is a genuine, active political project, not merely performative. The metrics reflect a sustained, high-stakes contest over legal authority and cultural identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secular-modernist coalition, this is a necessary and progressive move towards a modern, equitable society (a Rope or even a Mountain of constitutional principle). From the perspective of minority religious communities, it is a coercive imposition that erodes their fundamental rights and identity (a Snare). The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both the coordination claim and the extractive reality for those subject to it.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular-modernist coalition and the state legislature are clear beneficiaries (low directionality), gaining legal uniformity, national integration, and expanded state power. Minority religious communities and their personal law boards are the primary targets (high directionality), facing the loss of their traditional legal systems and identity-locked exit options. Gender equality advocates are also beneficiaries, as they see the UCC as a path to greater equality. The constitutional court acts as an observer, mediating the contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national integration, equality) is still 'live' for its beneficiaries, but its persistence is increasingly seen by victims as a means of cultural assimilation rather than genuine coordination. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination narrative and the genuine belief in its necessity by proponents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_pluralism,
    'Is legal pluralism in family law a transitional anomaly to be eliminated, or a legitimate and enduring feature of a diverse society?',
    'Long-term societal acceptance and political stability of pluralistic systems in other diverse democracies, or a constitutional amendment explicitly affirming or denying the right to personal law.',
    'If pluralism is deemed legitimate, the secularist reading''s extractiveness would be re-evaluated as an unjustified imposition, potentially reclassifying it towards a Snare. If it is confirmed as an anomaly, the Tangled Rope classification would be reinforced, with the extraction seen as a necessary cost of transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_pluralism, conceptual, 'Whether legal pluralism is a temporary state or a valid societal structure.').

omega_variable(
    gender_equality_impact_of_ucc,
    'Would a Uniform Civil Code genuinely advance gender equality across all communities, or would it merely impose majoritarian norms while potentially creating new forms of inequality?',
    'Empirical studies of UCC implementation in other contexts, disaggregated by community and gender, measuring actual changes in women''s rights and social status.',
    'If a UCC fails to deliver on gender equality, the ''beneficiary'' status of gender equality advocates would be challenged, potentially shifting the constraint''s overall extractiveness upward as a coordination failure. If it succeeds, it would strengthen the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_impact_of_ucc, empirical, 'The actual impact of a UCC on gender equality.').

omega_variable(
    identity_lock_strength,
    'How deeply are the identities of minority religious communities tied to their personal laws, and what would be the psychological and social cost of their elimination?',
    'Sociological and anthropological studies on identity formation within these communities, and qualitative research on the impact of legal changes on self-perception and community cohesion.',
    'A stronger identity-lock would amplify the effective extraction for these communities, reinforcing the Snare-like aspects of the Tangled Rope. A weaker lock might suggest more constrained exit options, but less existential threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which communal identity is fused with personal law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__secularist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__secularist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__secularist_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(marr_be_t10, marriage_authority__secularist_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(marr_be_t20, marriage_authority__secularist_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(marr_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(marr_be_t50, marriage_authority__secularist_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(marr_su_t10, marriage_authority__secularist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(marr_su_t20, marriage_authority__secularist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(marr_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(marr_su_t50, marriage_authority__secularist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel. Its high extractiveness and active enforcement distinguish it from other readings that prioritize communal autonomy or judicial harmonization. It directly challenges the 'communal_autonomy_reading' and 'federalist_millet_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
