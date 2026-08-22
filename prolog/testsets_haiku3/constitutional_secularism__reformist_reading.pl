% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: State Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint embodies a reformist reading of constitutional
 *   secularism: the state has an affirmative constitutional duty to eliminate
 *   religious practices that oppress marginalized groups—particularly
 *   scheduled castes (in the Indian constitutional context) and women within
 *   oppressive sects—even when doing so requires substantial interference
 *   with religious autonomy and the beliefs of religious conservatives. The
 *   constraint is read as liberatory when the beneficiary cohort (scheduled
 *   castes, women trapped in oppressive practices) is emphasized, and as
 *   coercive when the victim cohort (faith communities resisting state
 *   redefinition of their practices) is emphasized. This story instantiates
 *   the reformist reading; sibling readings (strict neutrality and principled
 *   intervention) would author different ε values and beneficiary/victim
 *   structures from the same constitutional kernel.
 *
 * KEY AGENTS:
 *   - Scheduled castes and oppressed subgroups: primary beneficiaries; state intervention dissolves group-internal hierarchies and oppressive practices.
 *   - Women in oppressive religious sects: secondary beneficiary cohort; intervention protects from harm enforced through religious authority.
 *   - Religious conservatives (across majority and minority communities): primary victims; state suppresses their religious autonomy in the name of protecting weaker sections.
 *   - State constitutional authority: agenda setter; defines which practices count as oppressive and enforces elimination.
 *   - Principled-intervention advocates: secondary beneficiaries; reformist reading provides legitimacy for paternalistic state action.
 *   - Strict-neutrality advocates: excluded voice; would argue the constraint violates constitutional equal distance and converts state into arbiter of religious correctness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.78).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "State Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, 'feaa0ca8-256b-4b97-a2d1-9f9dec1d7753').
narrative_ontology:cs_kernel_codification('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', distributed).
narrative_ontology:cs_authority_grounding('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', distributed).
narrative_ontology:cs_reading_relation('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', foundational, state_affirmative_duty_to_eliminate_oppressive_practices).
narrative_ontology:cs_axiom_status(state_affirmative_duty_to_eliminate_oppressive_practices, holdable).
narrative_ontology:cs_axiom_grounding('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', state_affirmative_duty_to_eliminate_oppressive_practices, deontological).
narrative_ontology:cs_axiom('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', foundational, human_rights_supremacy_over_religious_autonomy).
narrative_ontology:cs_axiom_status(human_rights_supremacy_over_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', human_rights_supremacy_over_religious_autonomy, deontological).
narrative_ontology:cs_axiom('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', secondary, reformist_ideology_as_constitutional_mandate).
narrative_ontology:cs_axiom_status(reformist_ideology_as_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', reformist_ideology_as_constitutional_mandate, instrumental).
narrative_ontology:cs_reference_frame('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', constitutional_secularism_as_emancipatory_project).
narrative_ontology:cs_drift_state('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', contemporary_mission_creep_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('feaa0ca8-256b-4b97-a2d1-9f9dec1d7753', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_oppressive_sects).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, religious_minorities_in_majority_dominated_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, faith_communities_resisting_state_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, principled_intervention_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_minorities_in_majority_dominated_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically oppressed by within-community hierarchy enforced through religious authority and practice. The constraint opens exit from internal oppression: women can pursue education, property ownership, occupational choice. The state intervention removes barriers that were previously enforced by religious community elites. They lack exit options within the community (identity-locked or physically trapped by kinship ties), making state intervention the only path to autonomy. The constraint does not dissolve the community—it restructures power within it.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, trapped, national).

% Trapped in sects or communities that restrict their autonomy through religious authority: enforced veiling, child marriage, restricted property rights, forced religious observance. The constraint provides legal backing for exit and for challenging community enforcement. They are identity-locked (leaving means losing family, community, social identity), making state sanction for exit-taking valuable. The constraint externalizes enforcement against within-group oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_in_oppressive_sects, beneficiary,
    powerless, biographical, identity_locked, national).

% In some cases, minority-community members who dissent from majority-enforced practices (e.g., lower-caste women in a majority-dominated caste-based community) gain protection from state intervention. However, the same constraint can also be weaponized against minority practices, making them simultaneously beneficiaries (when intervention protects their dissenting members) and payers (when intervention targets their practices as backward). Ambiguous position.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_minorities_in_majority_dominated_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, religious_minorities_in_majority_dominated_communities, payer).

% Faith communities and religious elites whose practices the state targets for elimination or modification. They experience the constraint as suppression of their autonomy claims, delegitimization of their traditions, and enforced reshaping of community life according to reformist ideology. Exit options are constrained: leaving the community means losing identity and social position, but staying means accepting state-enforced practice modification. They mount legal and political resistance to defend their autonomy against state redefinition.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, generational, constrained, national).

% Sets the boundaries of acceptable religious practice and enforces them through law and administrative sanction. Defines which practices count as oppressive and which merit protection. Collects legitimacy from its role as protector of the marginalized and arbiter of constitutional values. Bears enforcement costs and legitimacy challenges from religious conservatives and strict-neutrality advocates. Can modify the constraint's scope and definition unilaterally through judicial interpretation or legislative action.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_constitutional_authority, agenda_setter,
    institutional, generational, analytical, national).

% Constitutional scholars, judges, and rights advocates who argue the constraint violates constitutional secularism's core principle of equal distance from all religions. They would contend the state cannot be arbiter of which practices are oppressive without favoring some religious visions over others. Excluded from the decision-making process about which practices count as oppressive, but present in constitutional litigation and scholarly debate. Their position would yield a different constraint (strict_neutrality_reading) if adopted.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, strict_neutrality_advocates, excluded,
    moderate, generational, constrained, national).

% Reformist scholars, judges, and social-reform advocates who endorse the state's power to intervene in religious affairs when weaker sections are demonstrably harmed. Gain legitimacy and institutional influence from the constraint's operation. Distinguished from the reformist reading's core beneficiaries (scheduled castes, women) in that they hold this position ideologically rather than from within-group oppression. They are co-authors of the constraint's legitimacy framework.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, principled_intervention_advocates, beneficiary,
    powerful, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, principled_intervention_advocates, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, state_constitutional_authority).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protection of structurally oppressed subgroups (scheduled castes, women) within communities that enforce oppression through religious authority and practice. The constraint solves the collective-action problem of internal hierarchy: without state intervention, within-group elites can enforce oppression on weaker members who lack external exit options. The state provides an enforcement mechanism external to the community that allows weaker members to contest internal authority.
% TRANSFER_FUNCTION: Transfers religious autonomy from faith communities and religious conservatives to the state and to the protected subgroups. State gains authority to define acceptable practice; protected subgroups gain exit options and dignity. Religious conservatives lose autonomy to maintain unchanged practice and to enforce community norms on dissenting members.
% ABSENT_VOICES: Strict-neutrality advocates are structurally excluded: they would argue that the constraint violates constitutional equal distance and converts the state from neutral arbiter into advocate for reformist ideology. Also excluded: religious communities' own accounts of what counts as harm and what practices are central to identity (the state unilaterally defines oppression rather than negotiating it). Excluded: religious conservatives' claims that some 'oppressive practices' are genuinely central to faith and cannot be modified without destroying the tradition.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, scheduled castes and women currently protected by it would lose legal backing for exit and challenge. Internal community hierarchies would re-harden as state enforcement withdrew. Communities would reassert control over dissenting members and oppressive practices would likely intensify absent external protection. The entire landscape of community-internal power distribution would shift as state-backed rights protections evaporated.
% FOUNDING_PROBLEM: Religious communities in hierarchical societies enforce oppression on internal weaker sections—scheduled castes and women—through religious authority and practice. Caste hierarchies, child marriage, denial of property rights, forced religious observance, and seclusion are enforced as religious requirements. Weaker members lack exit options because community membership is identity-constitutive and economically essential. Without external enforcement, internal authority cannot be challenged.
% FOUNDING_PROBLEM_CORROBORATION: Scheduled castes, women's-rights advocates, and human-rights organizations outside the benefiting communities attest the problem is live: documented practices continue and cause ongoing harm. Religious conservative and strict-neutrality advocates contest whether these practices should be classified as oppressive or whether they are central to faith identity; this contestation does not change the fact that harm occurs under current community enforcement, only whether the state should intervene. Administrative data showing continued cases of child marriage, sati practices (in some regions historically), caste-based discrimination, and forced religious observance corroborate the founding problem's persistence.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.52→0.78 over interval) because the constraint's operation depends on state power to unilaterally declare practices oppressive and to enforce elimination regardless of the targeted community's assent. The constraint begins at moderate extraction (0.52) when limited to practices with unambiguous harm evidence (sati, child marriage), but rises sharply as scope expands to practices deemed oppressive by reformist ideology (dietary restrictions, gender roles, inheritance customs) where harm is contested or ideological. Theater ratio rises from 0.18 to 0.42, indicating growing divergence between the stated purpose (protecting weaker sections) and actual enforcement (controlling religious practice content). Suppression rises because enforcement requires active resistance to religious conservatives' countervailing claims to autonomy. The rise in suppression requirement matches the rise in extractiveness: as the constraint expands beyond narrow harm-based intervention, more active suppression is needed to hold it in place because religious communities' resistance intensifies. One shared time grid: every metric is authored at every time point (0, 5, 10, 15, 20, 25).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (scheduled castes, women) experiences this as liberation—removal of internal group hierarchy, access to exit, dignity restoration. The victim seat (religious conservatives) experiences this as coercive state intrusion into internal community governance, delegitimization of their practices, and suppression of their countervailing autonomy claims. The state agenda-setter seat experiences this as fulfilling a constitutional mandate to protect weaker sections. The divergence arises because the constraint simultaneously coordinates protection (for the beneficiary) and extracts autonomy (from the victim), and which effect dominates depends entirely on where you sit. The engine should compute markedly different types for these three seats: the state may see coordination/tangled_rope; the beneficiary may see snare (autonomy extraction from the victim, which gives them breathing room); the victim sees pure snare. The reformation clause means the beneficiaries genuinely benefit, which blocks pure-snare certification from the beneficiary seat—making the per-seat divergence a primary signal of the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes and women have low d (toward beneficiary): state intervention removes structural oppression, giving them exit options and dignity they lacked. The constraint subsidizes their position (negative effective extraction). Religious conservatives have high d (toward target): the state suppresses their autonomy claims and imposes redefinition of their practices under the threat of legal sanction. The state agent-setter has middling d, closer to beneficiary (0.25–0.35): it collects political legitimacy and institutional power from being the arbiter of correct practice, but also bears enforcement costs and legitimacy blowback. Strict-neutrality advocates, excluded from the conversation, would occupy high d (toward target) because the constraint violates their framework's core principle. No directionality overrides are needed; the structural derivation from beneficiary/victim + exit options is coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading explicitly declares that the state's founding problem was protection of weaker sections from internal group oppression—a genuinely live problem with documented harm. The constraint was built to solve that coordination problem (protect the harmed from within-group hierarchy) while simultaneously extracting autonomy from the intervening group. The mandatrophy risk is mission creep: does the affirmative duty scope remain bounded by documented harm to identifiable weaker sections, or does it expand to cover practices merely deemed backward by reformist ideology? The measurement series shows base_extractiveness rising faster than the harm-evidence base expands, suggesting the constraint is drifting toward ideology-driven suppression. Theater ratio rising (0.18→0.42) indicates performative maintenance is growing—state interventions increasingly target symbolic oppression (dress, dietary practice, inheritance form) rather than documented harm (violence, coerced participation). If this drift continues to the point where the constraint persists primarily to maintain reformist ideology's authority rather than to protect documented harm, mandatrophy triggers: the founding coordination problem is gone, but the extraction persists. The constraint would then reclassify from tangled_rope to piton (degraded function, theatrical maintenance) from the state's seat, though from the beneficiary's seat it might remain tangled_rope if real protections persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_vs_neutral_kernel_reading,
    'Is the kernel''s legitimacy grounded in state affirmative obligation to reform oppressive practices (reformist reading), or in state neutrality and equal distance from all religions (strict neutrality reading)?',
    'Constitutional text interpretation by competent authority; legislative history; case law trajectory tracking whether courts treat religious freedom as absolute or as subject to compelling state interest in protecting weaker sections.',
    'If reformist reading is adopted, the constraint computes as tangled_rope with affirmative beneficiary (scheduled castes, women). If neutrality reading prevails, the constraint reframes to snare (pure extraction masquerading as impartial regulation) or to mountain (equal-distance principle as natural constitutional law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformist_vs_neutral_kernel_reading, conceptual, 'Whether the constitutional kernel mandates state intervention in oppressive religious practices or mandates state neutrality.').

omega_variable(
    identity_locked_exit_for_exit_costs,
    'For religious conservatives whose practices the state targets for elimination, is their high exit cost (resistance to constraint) due to structural economic/legal barriers (trapped) or to identity-fusion with the practice itself (identity_locked)?',
    'Post-intervention trajectory: if suppression of the practice persists after legal barriers are removed and alternative income/community pathways are opened, the exit cost is internalized (identity_locked); if practice revives once legal barriers relax, the exit cost is structural (trapped).',
    'If identity_locked, the constraint''s effective suppression is higher than the authored scalar suggests — the victim carries the suppression internalized. If trapped, the constraint is purely structural coercion and may be more easily reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_for_exit_costs, empirical, 'Whether victim suppression is structural or internalized identity-fusion.').

omega_variable(
    marginalized_section_liberation_vs_autonomy_extraction,
    'Does the constraint genuinely liberate marginalized sections within communities (coordination + extraction bundled), or does it extract autonomy from religious groups under the cover of protecting weaker members?',
    'Counterfactual comparative: measure post-intervention agency and dignity trajectories of the protected group versus the intervened-upon group; track whether protected-group gains persist and whether they required continued suppression of intervened-upon group.',
    'If liberation is genuine and persists, the constraint is tangled_rope (coordination + extraction structurally coupled). If gains to protected group depend on ongoing suppression of interveners, the structure is snare (extraction masquerading as protection). If gains reverse after intervention pressure eases, the extraction was illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_section_liberation_vs_autonomy_extraction, empirical, 'Whether the constraint''s beneficiary gains are real or dependent on continued victim suppression.').

omega_variable(
    state_capacity_vs_legitimacy_overreach,
    'Does the state''s affirmative duty to eliminate practices extend only to practices causing objective documented harm, or does it extend to practices that reformist ideology deems backward regardless of harm measurement?',
    'Administrative practice tracking: examine what fraction of state interventions target documented harm to identifiable individuals versus target practices deemed oppressive by reformist ideology but with no clear harm allegant; measure variance across jurisdictions with different reformist strictness.',
    'If scope is narrow (documented harm), the constraint is more defensible as coordination (protecting the harmed). If scope is broad (ideology-driven), the constraint collapses toward snare (extraction of autonomy under humanitarian cover). This feeds the mandatrophy analysis: did the state duty mission creep beyond its founding problem?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_legitimacy_overreach, empirical, 'Whether affirmative duty scope is bounded by documented harm or expanded by reformist ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_sec_reform_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(const_sec_reform_tr_t5, constitutional_secularism__reformist_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(const_sec_reform_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(const_sec_reform_tr_t15, constitutional_secularism__reformist_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(const_sec_reform_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(const_sec_reform_tr_t25, constitutional_secularism__reformist_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(const_sec_reform_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(const_sec_reform_be_t5, constitutional_secularism__reformist_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(const_sec_reform_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(const_sec_reform_be_t15, constitutional_secularism__reformist_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(const_sec_reform_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(const_sec_reform_be_t25, constitutional_secularism__reformist_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(const_sec_reform_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(const_sec_reform_su_t5, constitutional_secularism__reformist_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(const_sec_reform_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(const_sec_reform_su_t15, constitutional_secularism__reformist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(const_sec_reform_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(const_sec_reform_su_t25, constitutional_secularism__reformist_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__reformist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the constitutional_secularism kernel. Reformist reading (this story) is the most extractive; it mandates state duty to eliminate oppressive practices. Strict neutrality reading treats secularism as equal distance (less extractive from religious conservatives, but also less protective of weaker sections). Principled intervention reading allows case-by-case state intervention (intermediate position). All three readings share the same constitutional text (the kernel) but decompose differently into constraint structures. The kernel itself is distributed (no single codification authoritatively resolves which reading is correct) and authority_grounding is distributed (competing parties claim legitimacy from the text). Each reading is generated as a separate story file with its own ε, beneficiary/victim structure, and computed type; the readings are linked here to show their mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
