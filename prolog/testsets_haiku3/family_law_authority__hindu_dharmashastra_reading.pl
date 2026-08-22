% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Marriage as Sacramental Samskara (Hindu Dharmashastra Reading)
 *   domain: family_law/religious_governance/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the Hindu dharmashastra reading of the
 *   contested kernel 'family_law_authority' — the authority to set the terms
 *   of marriage, divorce, property succession, and household authority. The
 *   reading treats marriage as a sacramental samskara (life-stage ritual)
 *   governed by brahmanical interpretation of dharmic texts (Manusmriti,
 *   Yajnavalkya Smriti) and customary practice in joint families. The
 *   sacrament is indissoluble except through brahmanical courts or rare
 *   ritual dissolution, and caste endogamy is integral to the reading's
 *   framework. Women enter marriage as mandatory ritual life-stage with
 *   identity fused to wifehood and the husband's lineage; exit is
 *   identity-annihilation and ritual violation. Joint-family property law is
 *   entangled with marital indissolubility. After 1955, the statutory Hindu
 *   Marriage Act attempted to displace this reading with a contractual one,
 *   but brahmanical authority, customary practice, and identity fusion
 *   persist in orthodox communities, courts, and temples. This is NOT a
 *   historical story about what was; it is the contemporary
 *   authority-conflict story about a reading that remains live even after
 *   statutory displacement.
 *
 * KEY AGENTS:
 *   - Brahmanical authority structures: control ritual validation and dissolution; interpret dharmashastra; benefit from authority preservation
 *   - Patriarchal household heads: exercise marital control; benefit from indissolubility and property asymmetry
 *   - Women as ritual participants: bear extraction costs; carry identity-lock through wife/mother/lineage roles
 *   - Lower-caste marriage seekers: excluded from intermarriage; trapped by caste endogamy enforcement
 *   - Divorced women post-1955: carry sacramental transgression status despite statutory override
 *   - Joint-family estate controllers: use property law entanglement to control women's exit
 *   - Secular state authority: structurally excluded by the reading's persistence through customary practice
 *   - Brahmanical women reformers: attempt to reinterpret dharma to support autonomy without abandoning the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Marriage as Sacramental Samskara (Hindu Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "family_law/religious_governance/political_theory").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '9f9e5ae5-1e7d-4511-8cf9-b46158306605').
narrative_ontology:cs_kernel_codification('9f9e5ae5-1e7d-4511-8cf9-b46158306605', fixed_text).
narrative_ontology:cs_authority_grounding('9f9e5ae5-1e7d-4511-8cf9-b46158306605', lineage).
narrative_ontology:cs_interpretation_layer_present('9f9e5ae5-1e7d-4511-8cf9-b46158306605').
narrative_ontology:cs_reading_relation('9f9e5ae5-1e7d-4511-8cf9-b46158306605', family_law_authority__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('9f9e5ae5-1e7d-4511-8cf9-b46158306605', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f9e5ae5-1e7d-4511-8cf9-b46158306605', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f9e5ae5-1e7d-4511-8cf9-b46158306605', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('9f9e5ae5-1e7d-4511-8cf9-b46158306605', foundational, marriage_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('9f9e5ae5-1e7d-4511-8cf9-b46158306605', marriage_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('9f9e5ae5-1e7d-4511-8cf9-b46158306605', foundational, caste_endogamy_ritual_purity).
narrative_ontology:cs_axiom_status(caste_endogamy_ritual_purity, holdable).
narrative_ontology:cs_axiom_grounding('9f9e5ae5-1e7d-4511-8cf9-b46158306605', caste_endogamy_ritual_purity, deontological).
narrative_ontology:cs_reference_frame('9f9e5ae5-1e7d-4511-8cf9-b46158306605', brahmanical_household_authority_pre_1955).
narrative_ontology:cs_drift_state('9f9e5ae5-1e7d-4511-8cf9-b46158306605', post_hindu_marriage_act_1955, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f9e5ae5-1e7d-4511-8cf9-b46158306605', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahmanical_authority_structures).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patriarchal_household_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_estate_controllers).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_marriage_seekers).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, divorced_women_post_1955).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce dharmashastra texts governing marriage as sacramental samskara. Control ritual validation, dissolution approval, and caste-marriage arbitration. Administer the legitimacy framework through which marriages are recognized as binding or void. In practice: brahmin priests validate marriages through ritual, brahmin courts (historically) or community councils (contemporarily) adjudicate disputes, brahmin scholars reinterpret texts to maintain authority through changing times. Benefit from preservation of this authority through control of household law, property succession, women's legal status, and caste boundaries.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmanical_authority_structures, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Exercise control over household property, marital dissolution (via repudiation where dharmashastra permits), caste endogamy enforcement, and wife's work and mobility. The constraint ensures wives cannot unilaterally seek divorce; succession laws tie property to patrilineal descent and joint-family ownership. Benefit from legal asymmetry in marital rights (husband can repudiate; wife cannot) and from treatment of wife's work and property as household assets. In practice: household heads control dowry receipt, remarriage decisions, property partition timing, wife's residence and employment, and caste-marriage approval.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patriarchal_household_heads, beneficiary,
    powerful, generational, mobile, regional).

% Enter marriage as mandatory ritual life-stage (samskara) that constitutes identity; cannot exit without spiritual transgression. Carry ritual obligations (puja, household ceremony, fertility management) that tie them to the household's authority. Bear extraction: cannot initiate divorce; dissolution is controlled by husband or (rarely) brahmanical courts. Property exclusion through joint-family succession rules. Identity fusion with husband's lineage makes exit identity-annihilation — divorce is treated as widow-status or ritual impurity. Carry the cost of patrilocal residence, name change, and permanent obligation to husband's household even if marriage is abusive.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants, beneficiary).

% Excluded from intermarriage with higher castes through caste endogamy rules enforced by the sacramental framework. Marriage prospects limited to caste boundaries; violation risks ritual pollution (ashaucha), social ostracism, and brahmanical denial of sacramental validation. The constraint's authority structure treats caste endogamy as integral to dharmic indivisibility — cannot reform marriage law (to allow unilateral divorce, for example) without challenging caste rules, and cannot challenge caste rules without questioning the entire brahmanical authority framework. Carry generational transmission of marriage market restriction.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_marriage_seekers, payer,
    powerless, generational, trapped, regional).

% After the 1955 Hindu Marriage Act, can legally divorce and remarry, but carry the sacramental reading's classification of divorce as ritual transgression. Face property disinheritance (joint-family succession rules persist in practice), social stigma in orthodox communities, and barrier to remarriage (treated as damaged goods, widow-adjacent status). The constraint's persistence through ritual authority and customary practice means formal legal divorce does not erase the sacramental transgression in many households, temples, and family courts. Carry the double-bind: statutory law permits exit, but the reading's authority structure penalizes it.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, divorced_women_post_1955, payer,
    moderate, biographical, constrained, national).

% Use joint-family property rules (Mitakshara and Dayabhaga schools, entangled with marital law) to control women's inheritance, succession, and economic exit. Wife has limited property rights in joint family (stridhan is hers, but joint-family property is husband's lineage's); divorce or widowhood does not guarantee inheritance. The constraint's authority structure treats property law as integral to marital law — reforming property rights (to permit women's inheritance of joint-family share) requires renegotiating the sacramental reading itself and brahmanical authority. Benefit from the entanglement: indissoluble marriage ensures women stay in household and do not claim partition; property rules ensure women cannot independently exit.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_estate_controllers, beneficiary,
    powerful, generational, mobile, regional).

% The post-1955 Indian state law framework (Hindu Marriage Act, Hindu Succession Act, Hindu Adoptions and Maintenance Act) attempted to displace the sacramental reading with a contractual one. Structurally barred from complete replacement in many communities, where customary religious authority persists in practice through family courts that recognize brahmanical mediation, temples that refuse to validate inter-caste marriages, and women who carry identity-lock despite statutory rights. The state has formal authority but practical displacement by the competing reading — courts recognize both (in practice creating dual authority). Excluded from speaking authoritatively in orthodox communities and temples.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_contractual_authority, excluded,
    institutional, generational, constrained, national).

% Challenge the sacramental reading from within the dharmic tradition, arguing that samskara implies holistic well-being (including women's autonomy and property rights) and that ritual authority must not entrench gender extraction. Attempt to reinterpret dharma to support women's exit rights, property claims, and choice in marriage without abandoning the religious-authority framework itself. Constrained exit: they cannot simply adopt the secular reading without losing religious legitimacy in the communities they seek to influence; they must argue within the dharmic tradition's own texts and logic.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmanical_women_reformers, observer,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, patriarchal_household_heads).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes marriage as a lifelong household unit through ritual sacramentalization (samskara). Provides a shared framework for property succession (joint family), ritual roles (wife's ceremonial authority), and child legitimacy. Coordinates expectations around marital stability through treating dissolution as violation of sacred obligation rather than contract termination.
% TRANSFER_FUNCTION: Moves women's labor (domestic, reproductive, ritual), property rights (via joint family), and bodily autonomy (marital obligations, no right to refuse intercourse) to the patriarchal household and brahmanical authority structures. Transfers control of marital dissolution from women to husbands and brahmanical courts. Enforces caste endogamy, which transfers marriage-alliance control upward to caste councils and away from individual choice.
% ABSENT_VOICES: Women who seek exit without brahmanical or husband approval; lower-caste marriage seekers who would marry across caste lines; divorced women whose sacramental status is permanent in the reading's own framework; children born outside the marriage's ritual validation (whose legitimacy status the constraint determines).
% DISAPPEARANCE_RATIONALE: If the sacramental reading and its enforcement mechanisms vanished, property succession would reorganize around individual testacy and spousal contracts rather than patrilineal joint-family succession. Women's exit from marriage would become possible without brahmanical or husband approval. Caste endogamy enforcement would lose its sacramental authority-backing (though customary pressure might persist). Household roles would be renegotiated between spouses rather than determined by ritual obligation. The entire structure of domestic authority would shift.
% FOUNDING_PROBLEM: Early agrarian household economies required stable, long-term property management and multigenerational coordination. Sacramentalizing marriage created an indissoluble framework that discouraged partition of joint-family estates and ensured household heads' authority over succession. Ritual justification (samskara as sacred obligation) made the arrangement feel inevitable rather than chosen, reducing enforcement costs.
% FOUNDING_PROBLEM_CORROBORATION: Brahmanical authorities and joint-family property controllers attest the problem persists: sacramental indissolubility is necessary for family stability and property preservation. Women reformers, secular legal scholars, and post-1955 state law attest the problem is functionally solved by contractual marriage and individual property law, and the sacramental reading persists as a rent-extraction mechanism (testimony from Indian women's commissions, academic analyses of Hindu Marriage Act implementation, and documented persistence of sacramental authority in family courts despite statutory override).
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint's operation transfers women's labor, property rights, bodily autonomy, and exit control to patriarchal household and brahmanical authority structures. Suppression is high (0.72 at endpoint, declining from 0.78 at start) because the constraint depends on active enforcement: ritual authority gatekeeping, customary ostracism, legal disability, and identity-lock internalization. Theater is moderate (0.41) — the constraint performs genuine coordination functions (property succession, household stability, child legitimacy) but an increasing share of enforcement energy defends the extraction (indissolubility, caste endogamy, wife's legal disability) rather than the coordination. The measurement series spans 70 time units roughly from pre-1955 through contemporary, showing suppression declining modestly as statutory displacement weakens enforcement capacity, while extraction remains stable because the constraint's core function (transferring women's autonomy and property to patriarchal household) persists through identity-lock and customary authority. Theater rises slightly post-1955 as brahmanical institutions emphasize 'spiritual authority' over 'legal authority' to maintain legitimacy despite statutory override.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (brahmanical authority, household heads), the constraint is genuine coordination — sacramentalizing marriage stabilizes multigenerational property and household authority through ritual obligation that feels inevitable. From the target seats (women, lower-caste marriage seekers), the same structure operates as enforced extraction. From the excluded seat (secular state law), the constraint persists through a competing authority framework (customary practice + ritual authority + identity-lock) that statutory override cannot fully displace. The engine should compute these as different classification outcomes — Rope from the brahmanical seat, Snare from women's seats, Piton from the state's seat (the brahmanical authority apparatus persists through theater/inertia even as statutory authority has formally displaced it).
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmanical authority and patriarchal household heads are structural beneficiaries (d ≈ 0.05–0.15; they collect extraction and control the rules). Women as ritual participants are the primary targets (d ≈ 0.85–0.95; they pay extraction, carry identity-lock, and have trapped exit). Lower-caste marriage seekers are targets (d ≈ 0.80–0.90; excluded from intermarriage, trapped by endogamy). Divorced women post-1955 are targets recovering toward beneficiary (d ≈ 0.60–0.75; they have exit via statute but carry sacramental stigma and constrained remarriage). Joint-family estate controllers are beneficiaries (d ≈ 0.10–0.20; they control property succession through the constraint). The secular state is an observer/excluded seat (d ≈ 0.50; it has formal authority but practical displacement by the competing reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (multigenerational household property management in agrarian economies requiring stable marital bonds) is CONTESTED per founding_problem_status. The disappearance_verdict is WORLD_REARRANGES (the constraint's absence would reorganize property, divorce rights, caste marriage rules, and household authority). The mismatch (contested founding problem + world_rearranges verdict) triggers mandatrophy analysis: does the constraint persist only because the problem is still live, or does it persist despite the problem being functionally solved? Post-1955 statutory law solves the founding problem contractually (individual testacy, voluntary marriage, unilateral divorce, no joint-family partition defaults) — yet the sacramental reading persists through customary authority, identity-lock, and ritual gatekeeping. This is classic Tangled Rope with mandatrophy latent (the founding problem is dead in statutory law, but the constraint persists through inertia, theater, and alternative authority structures). The constraint is NOT yet piton because brahmanical authority still actively enforces it (high suppression_requirement rather than mere theater), but it is zombie-flagged: statutes displaced the reading, but the reading continues via nonstate authority. A post-1955 reading where sacramental authority has been formally overridden (checked against the axiom_overriding omega) would be reclassified toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_persistence_post_statute,
    'After the 1955 Hindu Marriage Act formally displaced the sacramental reading with contractual law, does the identity-locked status of women remain through ritual authority and customary practice, or does statutory override actually break the identity fusion?',
    'Longitudinal study of women''s exit behavior and self-concept change post-divorce in communities where the sacramental reading remains practiced versus communities where the contractual reading dominates. Measure whether divorced women report identity-annihilation risk, decision-reversals due to ritual status anxiety, or constraint-resistance from family systems even when statute permits exit.',
    'If identity-locking persists despite statutory override, women remain trapped even under contractual law; suppression remains structural and internalized (identity-fusion). If it breaks, the statute''s displacement is more complete and suppression becomes purely structural (removable by law change). This determines post-1955 directionality: if identity persists, d stays high for women despite legal displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_statute, empirical, 'Persistence of sacramental identity-locking in women after statutory displacement of the dharmashastra reading.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the core premise of the dharmashastra sacramental reading (marriage is an indissoluble ritual binding that transcends individual will) logically foreclose the secular contractual reading (marriage is a dissolvable civil contract between autonomous individuals), or can both coexist as different parties'' readings?',
    'Examine whether courts, legislatures, or communities have attempted to hold both readings simultaneously. Test whether the statute''s adoption by the Indian state constitutes a hard foreclosure (one reading rules out the other in shared institutional framework) or social coexistence (different communities hold different readings without contradiction within each community). Documentary analysis of legislative debate (did they claim to displace or reform the reading?), judicial language (do family courts recognize both?), and community practice (which authority do people actually follow?).',
    'If foreclosure is real, this reading forecloses the contractual reading as a matter of logical structure (reading_relations: forecloses). If coexistence occurs despite statutory displacement, the relationship is ''influences'' — statute changes resource availability and social authority but both readings persist. This is the chief distinction test: coexistence validates the reading_relations choice and supports Tangled Rope classification (both readings operate, extraction persists despite statutory coordination attempt). Foreclosure would support Piton (one reading has won, other is dead).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the sacramental and contractual readings logically foreclose each other or coexist as different parties'' live readings.').

omega_variable(
    brahmanical_authority_gatekeeping,
    'Is brahmanical authority in marriage law (ritual validation, dissolution approval, caste-marriage arbitration) a genuine coordination function or a pure rent-extraction mechanism gatekeeping the sacramental reading itself?',
    'Compare communities that have stripped brahmanical gatekeeping (secular Hindu ceremonies, inter-caste marriages, unilateral divorce via state court) with communities that retain it. Measure whether coordination function (household stability, property succession clarity, child legitimacy) persists in the stripped version or degrades. If coordination persists without brahmanical gatekeeping, the authority is extractive. If it fails, authority is coordination-necessary.',
    'If coordination persists without brahmanical gatekeeping, the constraint is Tangled Rope: real coordination function + asymmetric extraction gatekeeping. If coordination fails without it, the constraint is Rope: the authority is coordination-necessary and extracted rents are legitimacy fees rather than pure theft. This affects mandatrophy interpretation: Rope that has persisted past its founding problem is acceptable inertia; Tangled Rope persisting past its founding problem is extractive zombie.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmanical_authority_gatekeeping, empirical, 'Whether brahmanical authority gatekeeping is coordination-necessary or pure rent-extraction mechanism.').

omega_variable(
    caste_endogamy_structural_entailment,
    'Is caste endogamy a structurally inseparable component of the sacramental reading, or can the sacramental indissolubility be maintained while opening caste boundaries?',
    'Examine reformist Hindu attempts to maintain sacramental marriage while rejecting caste endogamy rules (Brahmo Samaj, Arya Samaj, contemporary neo-Hindu theology, ISKCON inter-caste marriages). Assess whether the constraint''s enforcement structure (brahmanical authority, customary sanction, ritual validation) permits this decoupling or treats caste and sacrament as inseparable in doctrine and practice.',
    'If decoupling is possible, caste endogamy should be written as a DISTINCT constraint story with its own ε, beneficiary/victim structure, and classification (separate the two kernels: marital indissolubility vs. caste endogamy). If the reading''s authority structure enforces caste and sacrament as a package (refuse ritual validation for inter-caste marriages, treat caste crossing as sacramental violation), they form one constraint. This affects constraint-family decomposition and network relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_endogamy_structural_entailment, conceptual, 'Structural entailment of caste endogamy within the sacramental reading — are they one constraint or two?').

omega_variable(
    axiom_overriding_sacramental_indissolubility,
    'Has the foundational axiom of sacramental indissolubility (marriage_indissoluble_sacrament) been formally overridden within the brahmanical tradition itself, or does it persist as a live claim despite statutory displacement?',
    'Documentary analysis: (1) Official brahmanical statements on the 1955 Hindu Marriage Act — do brahmin organizations formally accept or reject contractual marriage? (2) Contemporary dharma-shastric commentary on divorce — do modern brahmin scholars maintain the sacramental axiom or adopt the statutory one? (3) Institutional practice — do major temples validate second marriages of divorced women or treat them as violating sacramental status?',
    'If the axiom is formally overridden (brahmanical tradition has adopted the contractual reading), the reading''s foundational claim is superseded (axiom status: overridden). The constraint would persist as Piton (zombie: atrophied function, maintained by inertia and theater). If the axiom persists (brahmanical tradition continues to assert sacramental indissolubility despite state law), the reading remains live and Tangled Rope (competing authority structures). This is the key test for mandatrophy: overridden axioms signal the reading is dead but persisting; holdable axioms signal it remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_sacramental_indissolubility, empirical, 'Whether the marriage_indissoluble_sacrament axiom remains holdable or has been formally overridden in brahmanical tradition.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (external: legal disability, ritual authority gatekeeping, economic dependency on joint family) or internalized (cognitive: identity-fusion, shame/honor norms, internalized obligation)?',
    'Post-exit trajectory study: measure women who have exited marriage (via divorce, widowhood, or informal separation) and track whether suppression persists after structural barriers are removed. If suppression persists (continued identity-annihilation risk, shame, decision-reversals), it is internalized. If it decays as women integrate into non-conjugal communities, it is structural.',
    'If suppression is primarily structural, it is stat-removable (reform enforcement machinery, open legal exit, strengthen property rights). If primarily internalized, the constraint carries psychological/cultural embedding that persists after formal displacement (identity_lock remains high even with statutory override). This informs strategy: structural suppression suggests legislative/enforcement reform; internalized suppression suggests cultural counter-narrative or community-level interventions required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized sources of suppression in women''s constrained exit from the sacramental reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fami_tr_t7, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 7, 0.39).
narrative_ontology:measurement(fami_tr_t14, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 14, 0.41).
narrative_ontology:measurement(fami_tr_t21, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 21, 0.42).
narrative_ontology:measurement(fami_tr_t28, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(fami_tr_t35, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(fami_tr_t42, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement(fami_tr_t49, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 49, 0.41).
narrative_ontology:measurement(fami_tr_t56, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 56, 0.41).
narrative_ontology:measurement(fami_tr_t63, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 63, 0.41).
narrative_ontology:measurement(fami_tr_t70, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 70, 0.41).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(fami_be_t7, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 7, 0.71).
narrative_ontology:measurement(fami_be_t14, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement(fami_be_t21, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 21, 0.62).
narrative_ontology:measurement(fami_be_t28, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement(fami_be_t35, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(fami_be_t42, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 42, 0.67).
narrative_ontology:measurement(fami_be_t49, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 49, 0.68).
narrative_ontology:measurement(fami_be_t56, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 56, 0.68).
narrative_ontology:measurement(fami_be_t63, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 63, 0.68).
narrative_ontology:measurement(fami_be_t70, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(fami_su_t7, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 7, 0.75).
narrative_ontology:measurement(fami_su_t14, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 14, 0.72).
narrative_ontology:measurement(fami_su_t21, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(fami_su_t28, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 28, 0.7).
narrative_ontology:measurement(fami_su_t35, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(fami_su_t42, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(fami_su_t49, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 49, 0.72).
narrative_ontology:measurement(fami_su_t56, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 56, 0.72).
narrative_ontology:measurement(fami_su_t63, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 63, 0.72).
narrative_ontology:measurement(fami_su_t70, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, joint_family_property_succession_india).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcement_hindu_communities).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five distinct constraint stories, one per major reading (dharmashastra, shariat, canonical, Parsi, secular). Each reading has different ε, beneficiary/victim structure, and classification. They share the same referent (the kernel — who has authority over marriage, divorce, property, and household roles) but instantiate different constraints via different readings. The dharmashastra reading FORECLOSES the secular contractual reading logically (indissoluble vs. dissoluble cannot coexist in the same framework), but they coexist socially as different parties' readings in contemporary India. Network edges represent institutional coupling and legislative/ritual conflict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
