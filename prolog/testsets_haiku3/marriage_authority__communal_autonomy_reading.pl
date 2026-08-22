% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Community Religious Authority over Marriage (Communal Autonomy Reading)
 *   domain: legal/constitutional/family law
 *
 * SUMMARY:
 *   This constraint embodies a legal pluralist arrangement in which marriage
 *   is governed by community religious authorities rather than uniform state
 *   family law. The communal-autonomy reading frames this as legitimate
 *   cultural self-determination: minority communities require autonomous
 *   authority over marriage to preserve distinct identity and practice.
 *   Religious leadership councils set and enforce rules; the state provides
 *   enforcement machinery without authoring the rules themselves. The
 *   arrangement is justified as a pluralist accommodation that prevents
 *   majoritarian erasure of minority law. The reading claims rope-type
 *   coordination (solving the community's need for unified marriage
 *   governance), but the authored metrics reflect substantial extraction
 *   (from dissenters and reformers) and rising theater (over 50 years,
 *   enforcement focus has shifted from functional rule-administration toward
 *   suppressing internal challenge). The claim/metric gap is structural and
 *   intentional: the communal-autonomy reading's own logic asserts rope
 *   functionality, while the measured operation shows increasing
 *   asymmetry—this divergence is what the corpus measures.
 *
 * KEY AGENTS:
 *   - Religious leadership councils (organized, arbitrage-grade exit) — set and interpret marriage law, amend rules through internal process, benefit from autonomy
 *   - Intra-community dissenters (powerless, identity-locked exit) — bound by rules they contest, cannot exit without losing identity, face enforcement
 *   - Women seeking reform (moderate power, constrained exit) — bear material costs of asymmetric rules, can organize but cannot override community rules without state action
 *   - State enforcement apparatus (institutional, analytical) — enforces rules authored by religious leadership, dual role: enforcer and constitutional guardian
 *   - Constitutional court (institutional, analytical) — reviews constitutionality of personal law, defers to pluralism doctrine in this reading
 *   - Constitutional equality advocates (organized, excluded) — barred from rule-authorship, petition for uniform civil code or constitutional floors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Community Religious Authority over Marriage (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal/constitutional/family law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '8fc49fd1-a523-4e82-b473-02d853f4fe6d').
narrative_ontology:cs_kernel_codification('8fc49fd1-a523-4e82-b473-02d853f4fe6d', formalized).
narrative_ontology:cs_authority_grounding('8fc49fd1-a523-4e82-b473-02d853f4fe6d', lineage).
narrative_ontology:cs_interpretation_layer_present('8fc49fd1-a523-4e82-b473-02d853f4fe6d').
narrative_ontology:cs_reading_relation('8fc49fd1-a523-4e82-b473-02d853f4fe6d', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('8fc49fd1-a523-4e82-b473-02d853f4fe6d', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fc49fd1-a523-4e82-b473-02d853f4fe6d', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('8fc49fd1-a523-4e82-b473-02d853f4fe6d', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('8fc49fd1-a523-4e82-b473-02d853f4fe6d', foundational, minority_cultural_autonomy_requires_legal_pluralism).
narrative_ontology:cs_axiom_status(minority_cultural_autonomy_requires_legal_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('8fc49fd1-a523-4e82-b473-02d853f4fe6d', minority_cultural_autonomy_requires_legal_pluralism, deontological).
narrative_ontology:cs_axiom('8fc49fd1-a523-4e82-b473-02d853f4fe6d', foundational, religious_community_authority_legitimate_without_state_authorship).
narrative_ontology:cs_axiom_status(religious_community_authority_legitimate_without_state_authorship, holdable).
narrative_ontology:cs_axiom_grounding('8fc49fd1-a523-4e82-b473-02d853f4fe6d', religious_community_authority_legitimate_without_state_authorship, deontological).
narrative_ontology:cs_reference_frame('8fc49fd1-a523-4e82-b473-02d853f4fe6d', constitutional_pluralism_grounded_in_minority_autonomy).
narrative_ontology:cs_drift_state('8fc49fd1-a523-4e82-b473-02d853f4fe6d', contemporary_gender_equality_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fc49fd1-a523-4e82-b473-02d853f4fe6d', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership_councils).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, women_seeking_reform).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, cultural_pluralism_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, minority_community_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The recognized authority that interprets and applies marriage law for their religious community. They set eligibility rules, solemnization procedures, grounds for dissolution, maintenance obligations, and inheritance entitlements. They can amend rules through internal processes (fatwa councils, synods, assembly decisions) without requiring state legislative approval. The state enforces their decisions (registering marriages, enforcing maintenance orders, adjudicating succession) while leaving rule-authorship to the community. They benefit from decision-making autonomy and the legitimacy that community-source rules confer on their authority.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership_councils, agenda_setter,
    organized, generational, arbitrage, regional).

% Members of the community who reject or contest the religious leadership's interpretation but are bound by its rules because exit means losing religious identity, community membership, and often economic/social support structures. They cannot appeal to state law to override community rules without effectively leaving the community. Women pursuing divorce against community religious law, members who remarry against tradition, individuals in interfaith relationships — all face enforcement by community authorities backed by state machinery. Their identity is constituted through community membership, making exit a form of social death.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, regional).

% Bear the material costs of personal law rules: restricted divorce rights, unequal inheritance, mandatory maintenance through religious channels only, guardianship restrictions on property and children. They participate in the community (beneficiary dimension) but face asymmetric rules that constrain their autonomy relative to men. Cannot access secular divorce or equal inheritance without state legislative action that requires amending personal law through community consent.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_seeking_reform, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, women_seeking_reform, beneficiary).

% Registers marriages and manages dissolution under personal law codes. Enforces maintenance awards, succession determinations, and guardianship decisions issued by religious authorities. Does not author the substantive rules but provides the coercive machinery to make them stick. Occupies a dual position: obligated to enforce law equally under constitutional supremacy, yet institutionally committed to recognizing religious community authority. This creates structural tension between enforcer and agenda-setter roles.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_enforcement_apparatus, observer).

% Civil rights organizations, gender-equality advocates, and secular legal reformers outside the religious communities are structurally barred from the community's rule-making process. They can petition the state or file constitutional challenges in secular courts, but their voice in the actual rules that govern community members is zero. They would advocate for uniform civil code and constitutional floor on equality, but the communal-autonomy reading excludes them from the legitimacy chain.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_equality_advocates, excluded,
    organized, generational, constrained, national).

% Reviews constitutional challenges to personal law rules and can impose constitutional floors (e.g., marriage consent, non-discrimination). In the communal-autonomy reading, the court defers to community rules as consistent with constitutional pluralism protections, intervening only at clear constitutional violation. The court sees itself as guardian of the pluralism arrangement rather than architect of uniform law, creating structural distance from equality-based review.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership_councils).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of uniform marriage governance within the community: one set of eligibility rules, recognized solemnization authority, unified grounds for dissolution, centralized maintenance and succession administration—avoiding chaos of individual contract and ensuring legitimacy through community source. Enables the community to maintain internal coherence across generations without state-mandated family law homogenization.
% TRANSFER_FUNCTION: Moves authority over family formation, dissolution, maintenance, and succession from the state to religious leadership councils; moves legitimacy claims from legislative supremacy to community tradition; moves dissent costs onto intra-community members who cannot exit without identity loss. The arrangement transfers decision-making power TO the religious councils (beneficiaries) and constraint-bearing power FROM the state TO the communities.
% ABSENT_VOICES: Intra-community women and dissenters are structurally present but subaltern (voiced through formal channels they do not control). Completely absent: constitutional-equality advocates, gender-rights litigants, and secular-law reformers who are excluded from rule-authorship even when the rules bind community members. Also absent: intra-community reform movements that seek change through internal challenge rather than state judicial override—their voice exists only as pressure the leadership must suppress.
% DISAPPEARANCE_RATIONALE: If community religious authority over marriage vanished overnight, a uniform civil code would govern all marriages, community-authored rules would have zero legal force, religious solemnization would remain ceremonial but not legally constitutive, and divorce, maintenance, and succession would follow state law uniformly. The community's ability to maintain internal legal pluralism and enforce its own rules would dissolve; leadership authority would lose its coercive backing. Communities would reorganize around either accepting secular law or creating informal (unenforceable) parallel systems.
% FOUNDING_PROBLEM: Constructed pluralism: recognition of religious community authority over personal law emerged from colonial-era accommodation of religious minorities and was institutionalized at independence as a consociational compromise to prevent majoritarian Hindu law from erasure of minority practice. The founding premise is that minority communities' survival depends on legal autonomy in domains central to identity (marriage, inheritance, community membership).
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership and communal-autonomy advocates attest the founding problem is live: minorities require legal space to maintain distinct practices. Constitutional equality advocates and gender-rights reformers attest the founding problem is superseded by constitutional equality guarantees and by evidence that personal law variation perpetuates gender inequality. Judicial review bodies have split: some defer to pluralism, others impose constitutional floors. No external corroboration exists for a simple resolution—the contest itself is the founding problem's evolution.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measurement tracks two offsetting dynamics: (1) the genuine coordination benefit for the community (low extraction if measured purely as administrative efficiency), but (2) the asymmetric costs borne by dissenters and women reformers who cannot exit identity-locked relationships or challenge rules. The authored ε=0.62 reflects this hybrid: real coordination for the beneficiary seat (religious leadership), real extraction for the victim seats (dissenters, reform-seeking women). The measurement series shows extractiveness rising from 0.48 to 0.62 over 54 years, not because the rules themselves changed uniformly, but because intra-community challenge intensified (women's reform movements, constitutional equality litigation) and the enforcement response hardened. Theater ratio rising from 0.12 to 0.28 indicates that an increasing share of leadership's effort went to suppressing internal dissent rather than administering rules—a diagnostic of Goodhart drift where the performance of rule-authority maintenance became more prominent than actual functional rule-application. Suppression requirement rising from 0.58 to 0.71 shows that enforcement intensity increased over the interval. The time-grid is shared: every metric is authored at the same five time points (1970, 1985, 2000, 2012, 2024), enabling coherent multi-metric temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   Religious leadership seats and dissenters/reformer seats experience radically different constraint classifications. From the leadership seat: this is a genuine rope (solves collective-action problem of unified community marriage law, low suppression—just administering rules, minimal theater). From the dissenter seat: this is a tangled-rope-to-snare transition (started as coordination forced-membership, evolving toward pure extraction as internal challenge was met with suppression rather than accommodation). The engine computes per-seat classifications from the structural data: leadership gets d near 0.0 (beneficiary), dissenters get d near 1.0 (target). Their computed types diverge. This divergence is the measurement: a constraint whose claim is rope but whose computed type varies by 1.5+ categories across seats is how structural asymmetry enters the record.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership councils are beneficiaries (d ≈ 0.1–0.2): they gain decision-making autonomy, legitimacy through rule-authorship, and coercive enforcement of their decisions by the state. Exit options are arbitrage-grade: they could negotiate with the state, relocate, or transition to informal enforcement; they are not trapped. Intra-community dissenters are victims (d ≈ 0.85–0.95): they bear the costs of rules they contest, cannot exit without identity loss (identity_locked), and face enforcement designed to prevent their voice from overriding leadership. Women seeking reform occupy the middle (d ≈ 0.55–0.65): they gain community membership and some inheritance/maintenance protections (beneficiary dimension), but face asymmetric rules that constrain autonomy and cannot reform rules without state intervention (payer dimension). State enforcement apparatus sits near symmetric (d ≈ 0.5): it bears the institutional cost of dual fidelity (enforcing personal law while respecting constitutional supremacy) and the reputational cost of enforcing rules it does not author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (minority legal autonomy preservation) has not cleanly died, but its relationship to the constraint has inverted. In 1970, the arrangement solved the stated founding problem: minorities could maintain distinct law. By 2024, the same arrangement is increasingly used to suppress internal reform (theater_ratio doubled), the coordination function is uncontested (no one disputes that unified rule is useful), but the question has shifted to: whose rules? The constraint exhibits mandatrophy-adjacent symptoms: the founding justification (prevention of majoritarian erasure) is live for religious leadership but contested/dead for intra-community women and dissenters who argue they are the ones being erased by rule enforcement. The classification stays rope because the coordination function is real and the leadership is a genuine beneficiary, but the rising extraction asymmetry and theater ratio indicate that the constraint is undergoing decomposition: the coordination story (real) and the extraction story (rising) are increasingly separable, pointing toward a future tangled-rope classification if the asymmetry widens further. Mandatrophy has not fully resolved because the founding problem statement itself is now contested—no single answer to 'is minority autonomy being preserved?' works across all seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_internalization,
    'Is the measured suppression of intra-community dissenters structural (legal barriers, economic exclusion, institutional barriers to exit) or internalized (dissenters believe they deserve the constraints, have accepted their subaltern status, or are isolated from reality-testing alternatives)?',
    'Post-exit trajectory analysis: track individuals who leave religious communities and adopt secular marriage arrangements. If suppression persists cognitively/behaviorally after legal barriers are removed (feelings of shame, identity fragmentation, continued deference to old leadership), suppression is partially internalized. If individuals rapidly adopt new frameworks, suppression is primarily structural.',
    'If suppression is mostly internalized, the constraint''s effective suppression is higher than the structural measure suggests—the targets carry the suppression with them even after exit and cannot fully re-equilibrate. This would support a snare classification over rope. If suppression is mostly structural, the rope classification holds: exit is available to identity-mobile individuals; those who remain are choosing constrained benefits over autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether suppression of intra-community dissenters is structural or internalized mechanism.').

omega_variable(
    kernel_reading_bifurcation,
    'Is the communal-autonomy reading a genuine codification of minority self-determination doctrine, or is it an ex-post justification for a system that actually emerged from colonial-era power structures and survives through leadership interest in preserving authority, with the pluralism framing as cover?',
    'Genealogical analysis of personal law institutionalization: examine founding documents, parliamentary debates at independence, early court judgments. Distinguish between (a) pluralism chosen by communities as autonomous decision, (b) pluralism imposed by colonial structures and retained by post-colonial elites, (c) pluralism accepted by communities as least-bad option given majoritarian alternatives.',
    'If (a): communal-autonomy reading is accurate; the constraint is rope grounded in genuine community choice. If (b) or (c): the constraint is snare disguised as rope; leadership benefits from a system they did not create but now defend. The ε value and beneficiary structure remain the same, but the classification shifts from rope-with-democratic-backing to snare-with-traditional-cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_bifurcation, conceptual, 'Whether communal-autonomy reading represents genuine community choice or ex-post legitimation of inherited colonial structures.').

omega_variable(
    gender_reform_exit_feasibility,
    'Can women seeking marriage reform exit the constraint via state judicial intervention (petitioning secular courts for divorce, filing constitutional equality challenges), or are state courts de facto closed to them due to procedural barriers, cost, or institutional resistance to overriding religious law?',
    'Empirical study of women''s access to secular courts: track litigation patterns, success rates of constitutional challenges to personal law, time-cost-emotional burden of pursuing secular remedies while remaining community members. Examine whether state courts treat personal law as presumptively binding or as subject to equality review.',
    'If state courts are genuinely open and responsive, women''s exit_options are constrained (not trapped); they can leverage state machinery against religious leadership. This supports a rope-to-tangled-rope classification: women have a partially effective alternative. If state courts defer to personal law or impose prohibitive procedural barriers, exit is trapped; women are truly locked into the religious system. This supports a snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_reform_exit_feasibility, empirical, 'Whether women can use state judicial intervention to exit or reform personal law constraints.').

omega_variable(
    communal_unanimity_fiction,
    'Do religious leadership councils make rules through genuinely democratic community process (assemblies, councils with representational breadth), or do they exercise oligarchic authority claimed to represent community will but actually constrained only by informal resistance pressure?',
    'Institutional analysis of rule-making procedures: examine representation structures of leadership councils, opportunities for internal challenge and amendment, gender/generational composition, documented instances of rule change driven by community petition vs. leadership discretion alone.',
    'If democratic: the rope classification holds; communities have voice in rule formation even if dissenters remain constrained. If oligarchic: the constraint shifts toward snare; leadership authority is asserted in the name of community but actually rests on control of interpretation machinery and enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_unanimity_fiction, empirical, 'Whether communal rule-making is genuinely democratic or oligarchic leadership claiming community authority.').

omega_variable(
    pluralism_vs_exemptionism,
    'Is this constraint a true pluralism reading (multiple legitimate legal systems coexisting in public law) or an exemptionism reading (one legal system with carve-outs for religious minorities)?',
    'Constitutional interpretation: examine how courts frame the authority—as constitutional recognition of plural systems on equal ground, or as state-granted exemption from uniform law. Examine whether Hindu law is equally ''personal law'' subject to state enforcement or whether it is treated as presumptively uniform and only religious-minority law is personal law.',
    'If true pluralism: each system has equal constitutional standing. If exemptionism: religious-minority law is subordinate to the presumptive state framework, making dissent within the minority system more constrained than dissent in the majority system. This affects whether the classification is rope (pluralism) or snare (exemption-as-cover-for-subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralism_vs_exemptionism, conceptual, 'Whether the constraint embodies plural legal systems or state-granted exemptions to uniform law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__communal_autonomy_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1970, observed).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__communal_autonomy_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement_basis(marr_tr_t1985, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority__communal_autonomy_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority__communal_autonomy_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement_basis(marr_tr_t2012, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__communal_autonomy_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1970, marriage_authority__communal_autonomy_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(marr_be_t1970, observed).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__communal_autonomy_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement_basis(marr_be_t1985, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority__communal_autonomy_reading, base_extractiveness, 2000, 0.59).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2012, marriage_authority__communal_autonomy_reading, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement_basis(marr_be_t2012, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__communal_autonomy_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(marr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1970, marriage_authority__communal_autonomy_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement_basis(marr_su_t1970, observed).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__communal_autonomy_reading, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement_basis(marr_su_t1985, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority__communal_autonomy_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2012, marriage_authority__communal_autonomy_reading, suppression_requirement, 2012, 0.69).
narrative_ontology:measurement_basis(marr_su_t2012, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__communal_autonomy_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(marr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five structurally distinct constraints, one per reading. This constraint (communal_autonomy_reading) instantiates the reading in which marriage authority belongs to religious communities recognized under constitutional pluralism doctrine. The sibling constraints instantiate the secularist_reading (authority belongs to legislature), gender_rights_reading (authority contested on equality grounds), judicial_harmonization_reading (authority evolving via constitutional review), and federalist_millet_reading (authority deliberately fragmented as anti-majoritarian structure). Each reading has different ε, beneficiaries, victims, and computed type. They are not variants of one constraint; they are separate constraints sharing a kernel. The network links track how changes in one reading affect the structural position of siblings (e.g., if judicial harmonization imposes constitutional floors, it constrains communal_autonomy space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
