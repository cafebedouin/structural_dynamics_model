% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Marriage as Sacramental Samskara under Dharmashastra Authority
 *   domain: religious_governance/family_law/south_asian_legal_history
 *
 * SUMMARY:
 *   This constraint story captures the Hindu Dharmashastra reading of the
 *   family_law_authority kernel: marriage as sacramental samskara (sacrament)
 *   governed by Vedic and Smriti texts (Manusmriti, Yajnavalkya, Narada,
 *   etc.) and customary practice. The constraint operates through three
 *   interlocking mechanisms: (1) sacramental indissolubility — marriage
 *   creates an irreversible ritual bond dissolvable only by death (pre-1955);
 *   (2) caste endogamy norms — marriage must occur within varna/jati
 *   boundaries to preserve ritual purity and pitr-rina (ancestral debt)
 *   continuity; (3) joint family property rules (Mitakshara/Dayabhaga) —
 *   wives enter as ritual participants in the husband's coparcenary, not as
 *   autonomous property holders. The 1955 Hindu Marriage Act introduced
 *   statutory divorce and reformed property rights, but customary enforcement
 *   through caste panchayats, khap councils, and family pressure persists,
 *   creating a dual regime where formal law and dharmic custom coexist in
 *   tension. The constraint is claimed as tangled_rope: it coordinates joint
 *   family continuity, ancestral debt discharge, and ritual order (rope
 *   function) while extracting asymmetrically from women, lower castes, and
 *   out-marriage seekers through identity-locked suppression (snare
 *   function).
 *
 * KEY AGENTS:
 *   - brahminical_interpretive_authority: agenda_setter (institutional/identity_locked) — controls textual interpretation, ritual validation, and customary enforcement
 *   - patriarchal_joint_family_heads: agenda_setter/beneficiary (powerful/identity_locked) — administer joint family property, control marriage negotiations, collect ritual/economic benefits
 *   - caste_endogamy_enforcers: agenda_setter (organized/identity_locked) — khap panchayats, caste associations, community elders who police marriage boundaries
 *   - dharmashastra_scholastic_tradition: beneficiary (institutional/arbitrage) — derives authority, patronage, and institutional continuity from being the constraint's authorized interpreters
 *   - women_as_ritual_participants: payer (moderate/identity_locked) — bear reproductive, domestic, and ritual labor; denied autonomous property, exit, and contractual capacity
 *   - lower_caste_outmarriage_seekers: payer (powerless/trapped) — face violence, exclusion, and property forfeiture for crossing caste boundaries
 *   - widows_denied_remarriage: payer (moderate/trapped) — historically denied remarriage, property rights, and social standing; ritualized as inauspicious
 *   - daughters_excluded_joint_property: payer (moderate/constrained) — excluded from coparcenary property under Mitakshara until 2005 amendment; customary exclusion persists
 *   - reformist_legal_activists: observer (analytical/analytical) — litigate statutory reform, challenge customary enforcement, document extraction patterns
 *   - secular_state_courts: observer (institutional/analytical) — adjudicate between statutory law and customary practice; sometimes enforce, sometimes undermine dharmic authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.72).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Marriage as Sacramental Samskara under Dharmashastra Authority").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious_governance/family_law/south_asian_legal_history").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, 'c714fef0-442e-4fc1-87a6-bd3bf6f89e50').
narrative_ontology:cs_kernel_codification('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', formalized).
narrative_ontology:cs_authority_grounding('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', lineage).
narrative_ontology:cs_interpretation_layer_present('c714fef0-442e-4fc1-87a6-bd3bf6f89e50').
narrative_ontology:cs_reading_relation('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', foundational, marriage_as_irreversible_samskara).
narrative_ontology:cs_axiom_status(marriage_as_irreversible_samskara, overridden).
narrative_ontology:cs_axiom_grounding('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', marriage_as_irreversible_samskara, deontological).
narrative_ontology:cs_axiom('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', foundational, varna_endogamy_as_ritual_necessity).
narrative_ontology:cs_axiom_status(varna_endogamy_as_ritual_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', varna_endogamy_as_ritual_necessity, deontological).
narrative_ontology:cs_axiom('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', foundational, wife_as_ritual_participant_not_contractor).
narrative_ontology:cs_axiom_status(wife_as_ritual_participant_not_contractor, overridden).
narrative_ontology:cs_axiom_grounding('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', wife_as_ritual_participant_not_contractor, deontological).
narrative_ontology:cs_axiom('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', secondary, coparcenary_property_excludes_daughters).
narrative_ontology:cs_axiom_status(coparcenary_property_excludes_daughters, overridden).
narrative_ontology:cs_axiom_grounding('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', coparcenary_property_excludes_daughters, conventional).
narrative_ontology:cs_reference_frame('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', classical_dharmashastra_patrlineal_order).
narrative_ontology:cs_drift_state('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', post_hindu_code_bills_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c714fef0-442e-4fc1-87a6-bd3bf6f89e50', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_interpretive_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patriarchal_joint_family_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcers).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, dharmashastra_scholastic_tradition).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_outmarriage_seekers).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, daughters_excluded_joint_property).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, marriage_as_sacramental_samskara).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, varna_dharma_as_cosmic_order).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, pitr_rina_ancestral_debt_continuity).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, kanyadana_as_irreversible_gift).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls textual interpretation of Dharmashastras, validates ritual correctness, authorizes customary enforcement through caste councils. Derives authority from claimed continuity with Vedic revelation and guru-parampara. Cannot exit without losing the epistemic position that constitutes their authority — the role and the person are fused in the tradition's own self-understanding.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_interpretive_authority, agenda_setter,
    institutional, generational, identity_locked, continental).

% Administer joint family property (coparcenary), negotiate marriages within caste boundaries, perform ancestral rites (shraddha) that require a wife as ritual participant. Collect economic control over family assets, ritual status, and social authority. Their identity as 'karta' (family head) and 'grihastha' (householder) is constituted by the constraint — exit would mean ritual failure and social death.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patriarchal_joint_family_heads, agenda_setter,
    powerful, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, patriarchal_joint_family_heads, beneficiary).

% Khap panchayats, caste associations, community elders who police marriage boundaries through social boycott, violence, and ritual excommunication. Their authority derives entirely from the constraint's persistence — they are the enforcement arm of caste endogamy. Exit would dissolve their organizational basis.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcers, agenda_setter,
    organized, biographical, identity_locked, regional).

% Pandits, acharyas, matha heads, and academic scholars who interpret, teach, and litigate Dharmashastra. Receive patronage, institutional positions, and epistemic authority from being the constraint's authorized interpreters. Unlike the enforcers, they have arbitrage-grade exit: they can shift to secular academia, comparative religion, or other traditions while retaining scholarly capital.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dharmashastra_scholastic_tradition, beneficiary,
    institutional, generational, arbitrage, continental).

% Enter marriage through kanyadana (irrevocable gift) as ritual participants in husband's ancestral rites. Bear reproductive labor, domestic management, and ritual obligations (vrata, shraddha assistance). Denied autonomous property (stridhana limited), divorce access (pre-1955), and contractual capacity. Exit requires ritual/social death: loss of ritual status, familial rupture, caste excommunication. The 'wife' identity is fused with the constraint — there is no 'woman' position outside it in the dharmic frame.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, women_as_ritual_participants, payer,
    moderate, biographical, identity_locked, local).

% Seek marriage across caste boundaries for love, mobility, or escape from caste oppression. Face honor violence, social boycott, property forfeiture, and ritual pollution declarations from khap panchayats. No exit within the constraint's frame — the only exits are physical flight (losing all social ties) or submission. State law formally protects inter-caste marriage but customary enforcement overrides it.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_outmarriage_seekers, payer,
    powerless, biographical, trapped, local).

% Historically denied remarriage, excluded from property inheritance, ritualized as inauspicious (ashubha), dependent on natal or marital family for survival. The 1856 Widow Remarriage Act and 1955 HMA formally changed this, but customary practice in many regions persists. Exit from widowhood stigma is blocked by the constraint's ritual logic: the sacramental bond survives husband's death, making remarriage ritually adulterous.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage, payer,
    moderate, biographical, trapped, local).

% Excluded from Mitakshara coparcenary property until 2005 Hindu Succession Act amendment; customary exclusion persists through family pressure, lack of awareness, and litigation costs. Even post-2005, daughters face social sanctions for claiming share. Exit from exclusion requires legal battle against own family — constrained by both legal process and identity loyalty.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, daughters_excluded_joint_property, payer,
    moderate, biographical, constrained, local).

% Feminist lawyers, anti-caste activists, human rights NGOs who litigate statutory reform (1955 HMA, 2005 HSA, domestic violence laws), document customary enforcement, and challenge the constraint's extraction. They operate from outside the dharmic frame, using constitutional equality as counter-framework. Their exit is analytical — they can shift frameworks at will.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reformist_legal_activists, observer,
    analytical, generational, analytical, national).

% High Courts and Supreme Court adjudicating between statutory Hindu law and customary practice. Sometimes uphold women's statutory rights (undermining dharmic authority), sometimes defer to custom (reinforcing it). Their rulings shape the constraint's operational extraction. Exit is analytical — they apply constitutional framework, not dharmic.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_state_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, patriarchal_joint_family_heads).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates three interlocking functions: (1) Discharge of pitr-rina (ancestral debt) through sacramental continuity of the patriline — the son's marriage produces the grandson who performs shraddha; (2) Joint family risk-pooling and property management through coparcenary (Mitakshara) — the undivided family holds property collectively, smoothing lifecycle risks; (3) Caste endogamy as ritual purity maintenance — preserves the varna/jati boundaries necessary for ritual efficacy and cosmic order (rita).
% TRANSFER_FUNCTION: Moves reproductive labor, domestic management, ritual service, and property rights from women to joint family heads and caste structures. Moves marriage alliance decisions from individuals to family heads. Moves authority over property, divorce, and ritual status from women to patriarchal/caste authorities. Moves interpretive authority and enforcement power to brahminical scholastic tradition and khap panchayats.
% ABSENT_VOICES: Dalit and Adivasi women facing intersectional caste-gender extraction — their voices are excluded from both the dharmic frame (which treats them as outside ritual order) and the reformist frame (which often centers savarna women's concerns). Historical widows who committed sati or lived as ascetic widows — their testimony is filtered through colonial/brahminical records. Contemporary women in rural areas who cannot access statutory remedies — they are not in the courtroom or the NGO meeting room.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight: joint family property would partition or shift to individual ownership; women would gain autonomous property, divorce, and remarriage access; caste endogamy would lose its ritual enforcement (though social prejudice would persist); brahminical interpretive authority would lose its family law jurisdiction; khap panchayats would lose their primary enforcement mandate. The South Asian kinship, property, and ritual order would fundamentally reorganize — though secular law and social inertia would shape the transition.
% FOUNDING_PROBLEM: Ensure sacramental continuity of the patriline to discharge pitr-rina (ancestral debt) through shraddha rites performed by a legitimate son born of a wife married within caste boundaries through kanyadana — thereby maintaining cosmic order (rita) and joint family integrity across generations.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalists (brahminical authorities, orthodox families, Hindu nationalist organizations) attest the problem is live: cosmic order, ancestral debt, and ritual efficacy are timeless; statutory law is adharmic interference. Reformists (feminist legal scholars, anti-caste activists, Ambedkarite tradition, women's movement) attest the problem is dead: pitr-rina is a patriarchal construct; individual autonomy, gender equality, and constitutional rights have superseded it. The Indian state's legislative record (1955 HMA, 1956 HSA, 2005 HSA amendment) formally treats the founding problem as dead — but customary enforcement treats it as live. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers women's labor, reproductive capacity, and property rights to joint family heads and caste structures while denying exit; suppression (0.68) is substantial because enforcement operates through identity-locked mechanisms (caste excommunication, ritual pollution, familial ostracism) that persist despite statutory reform; theater_ratio (0.28) reflects the genuine coordination function (joint family risk-pooling, ritual continuity, ancestral debt discharge) alongside the extractive core. Accessibility_collapse (0.76) is high because the sacramental frame makes alternatives (divorce, inter-caste marriage, female property ownership) conceptually unintelligible within the dharmic worldview — not just costly but category-violating. Resistance (0.42) is moderate: organized resistance exists (reform movements, feminist litigation, anti-caste activism) but operates against a constraint that has restructured the very categories through which resistance could be imagined.
 *
 * PERSPECTIVAL GAP:
 *   From the brahminical_interpretive_authority and patriarchal_joint_family_heads seats, the constraint appears as rope: it coordinates cosmic order (rita), discharges ancestral debts (pitr-rina), and maintains social cohesion through ritual continuity. From the women_as_ritual_participants and lower_caste_outmarriage_seekers seats, the same structure operates as snare: identity-locked suppression, denied exit, and asymmetric extraction of labor/property. The engine computes this divergence from the structural data — the declared beneficiaries/victims, power/exit profiles, and identity_locked exit_options generate different directionality (d) values, producing different effective extraction (χ) and thus different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (brahminical authority, joint family heads, caste enforcers, scholastic tradition) collect ritual authority, property control, social status, and institutional continuity — their directionality d is low (near 0.1-0.2) because the constraint subsidizes their position. Victims (women, lower-caste seekers, widows, daughters) bear reproductive/domestic labor, denied property, denied exit, and violence — their directionality d is high (near 0.8-0.9) because the constraint extracts from them. The identity_locked exit_option for women and lower-caste agents is critical: exit requires not just physical departure but ritual/social death (caste excommunication, loss of ritual status, familial rupture), making d structurally higher than for merely constrained agents. Caste_endogamy_enforcers are agenda_setters with identity_locked exit — they administer the constraint and their authority depends on its persistence, so they cannot exit without losing their structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discharging pitr-rina through sacramental continuity of the patriline) is contested: reformists argue it is dead (modern law, individual rights, women's autonomy have superseded it); traditionalists argue it is live (cosmic order, ancestral debt, and ritual efficacy are timeless). The constraint persists despite the 1955 statutory rupture because the mandate has been transmuted: from 'ensure sacramental continuity' to 'preserve caste hierarchy and patriarchal property control.' This is classic mandatrophy — the original coordination function (ritual continuity) has atrophied or been formally superseded, but the extraction function (caste/patriarchal rent) maintains the constraint through customary enforcement. The theater_ratio rise post-1955 (0.22→0.35) captures this: statutory reform created a formal layer that the constraint's operators perform compliance with while customary extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (hindu_dharmashastra_reading) of the contested kernel family_law_authority. What structural changes would sibling readings (muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading, secular_contractual_reading) instantiate in beneficiary/victim sets, extraction profiles, and enforcement logics?',
    'Produce separate constraint stories for each sibling reading with their own ε, stakeholders, and claimed_type; link via network.affects_constraints; compare computed per-seat classifications across the family.',
    'If sibling readings produce systematically different extraction/suppression profiles for structurally analogous agents (e.g., women, lower-status parties), the kernel-level contest is structural, not merely interpretive. This reading''s classification is stabilized only relative to its declared frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committers-frame: this constraint instantiates one reading of a contested kernel; sibling readings are other constraints, not perspectives within this one.').

omega_variable(
    sacramental_indissolubility_historical_break,
    'Does the 1955 Hindu Marriage Act''s introduction of divorce provisions represent a genuine constraint transition (tangled_rope → scaffold → rope) or a formal overlay on persistent sacramental enforcement in customary practice?',
    'Comparative analysis of post-1955 litigation patterns, customary council (khap/panchayat) rulings, and women''s exit trajectories in regions with strong vs. weak statutory penetration.',
    'If customary enforcement persists, the pre-1955 extraction profile (high suppression, high accessibility_collapse for women) continues de facto, making the statutory reform a theater layer. If statutory law displaced customary enforcement, the constraint''s type shifted toward rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_indissolubility_historical_break, empirical, 'Whether the 1955 statutory rupture changed the constraint''s operational extraction or only its formal presentation.').

omega_variable(
    caste_endogamy_extraction_vs_coordination,
    'Is caste endogamy enforced by this constraint a coordination mechanism (preserving ritual purity/joint family integrity) or an extraction mechanism (concentrating property/status in dominant castes, suppressing lower-caste mobility)?',
    'Measure inter-caste marriage suppression intensity and property/ritual exclusion patterns across caste strata; test whether endogamy rules bind dominant castes symmetrically or extract asymmetrically from lower castes.',
    'If asymmetric extraction dominates, the constraint''s claimed coordination function is cover for caste hierarchy maintenance, reinforcing tangled_rope classification. If coordination dominates symmetrically, the rope component strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_endogamy_extraction_vs_coordination, conceptual, 'Whether caste endogamy norms function as genuine coordination or as extraction cover within this constraint.').

omega_variable(
    wife_ritual_participant_vs_autonomous_agent,
    'Does the ''wife as ritual participant'' framing reflect a genuine dharmic role distinction (complementary but non-contractual) or a structural denial of agency that enables extraction of labor, reproductive capacity, and property rights?',
    'Trace property rights, divorce access, and ritual authority distributions in Dharmashastra commentaries vs. actual customary practice; compare with secular_contractual_reading''s autonomous-agent baseline.',
    'If ritual participant status systematically correlates with denied exit, property, and bodily autonomy, the framing is extractive cover. If ritual role carries distinct but enforceable protections (stridhana, maintenance, ritual authority), the coordination function has substantive content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wife_ritual_participant_vs_autonomous_agent, empirical, 'Whether the non-contractual wife role is a genuine dharmic coordination form or an extraction-enabling identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_hdr_tr_t1800, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(fla_hdr_tr_t1850, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(fla_hdr_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(fla_hdr_tr_t1950, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(fla_hdr_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.35).
narrative_ontology:measurement(fla_hdr_tr_t1980, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(fla_hdr_tr_t2000, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(fla_hdr_tr_t2024, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fla_hdr_be_t1800, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1800, 0.78).
narrative_ontology:measurement(fla_hdr_be_t1850, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(fla_hdr_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.73).
narrative_ontology:measurement(fla_hdr_be_t1950, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(fla_hdr_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.68).
narrative_ontology:measurement(fla_hdr_be_t1980, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(fla_hdr_be_t2000, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(fla_hdr_be_t2024, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fla_hdr_su_t1800, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1800, 0.82).
narrative_ontology:measurement(fla_hdr_su_t1850, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(fla_hdr_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(fla_hdr_su_t1950, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(fla_hdr_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.65).
narrative_ontology:measurement(fla_hdr_su_t1980, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(fla_hdr_su_t2000, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fla_hdr_su_t2024, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 2024, 0.68).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1800, tn=2024
narrative_ontology:measurement(fla_hdr_grid_01, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(class), 1800, 0.88).
narrative_ontology:measurement(fla_hdr_grid_02, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(class), 2024, 0.72).
narrative_ontology:measurement(fla_hdr_grid_03, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(individual), 1800, 0.92).
narrative_ontology:measurement(fla_hdr_grid_04, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(individual), 2024, 0.78).
narrative_ontology:measurement(fla_hdr_grid_05, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(organizational), 1800, 0.65).
narrative_ontology:measurement(fla_hdr_grid_06, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(organizational), 2024, 0.48).
narrative_ontology:measurement(fla_hdr_grid_07, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(structural), 1800, 0.72).
narrative_ontology:measurement(fla_hdr_grid_08, family_law_authority__hindu_dharmashastra_reading, accessibility_collapse(structural), 2024, 0.68).
narrative_ontology:measurement(fla_hdr_grid_09, family_law_authority__hindu_dharmashastra_reading, resistance(class), 1800, 0.18).
narrative_ontology:measurement(fla_hdr_grid_10, family_law_authority__hindu_dharmashastra_reading, resistance(class), 2024, 0.48).
narrative_ontology:measurement(fla_hdr_grid_11, family_law_authority__hindu_dharmashastra_reading, resistance(individual), 1800, 0.15).
narrative_ontology:measurement(fla_hdr_grid_12, family_law_authority__hindu_dharmashastra_reading, resistance(individual), 2024, 0.52).
narrative_ontology:measurement(fla_hdr_grid_13, family_law_authority__hindu_dharmashastra_reading, resistance(organizational), 1800, 0.25).
narrative_ontology:measurement(fla_hdr_grid_14, family_law_authority__hindu_dharmashastra_reading, resistance(organizational), 2024, 0.38).
narrative_ontology:measurement(fla_hdr_grid_15, family_law_authority__hindu_dharmashastra_reading, resistance(structural), 1800, 0.22).
narrative_ontology:measurement(fla_hdr_grid_16, family_law_authority__hindu_dharmashastra_reading, resistance(structural), 2024, 0.42).
narrative_ontology:measurement(fla_hdr_grid_17, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(class), 1800, 0.78).
narrative_ontology:measurement(fla_hdr_grid_18, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(class), 2024, 0.62).
narrative_ontology:measurement(fla_hdr_grid_19, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(individual), 1800, 0.85).
narrative_ontology:measurement(fla_hdr_grid_20, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(fla_hdr_grid_21, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(organizational), 1800, 0.45).
narrative_ontology:measurement(fla_hdr_grid_22, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(organizational), 2024, 0.38).
narrative_ontology:measurement(fla_hdr_grid_23, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(structural), 1800, 0.52).
narrative_ontology:measurement(fla_hdr_grid_24, family_law_authority__hindu_dharmashastra_reading, stakes_inflation(structural), 2024, 0.55).
narrative_ontology:measurement(fla_hdr_grid_25, family_law_authority__hindu_dharmashastra_reading, suppression(class), 1800, 0.82).
narrative_ontology:measurement(fla_hdr_grid_26, family_law_authority__hindu_dharmashastra_reading, suppression(class), 2024, 0.68).
narrative_ontology:measurement(fla_hdr_grid_27, family_law_authority__hindu_dharmashastra_reading, suppression(individual), 1800, 0.88).
narrative_ontology:measurement(fla_hdr_grid_28, family_law_authority__hindu_dharmashastra_reading, suppression(individual), 2024, 0.72).
narrative_ontology:measurement(fla_hdr_grid_29, family_law_authority__hindu_dharmashastra_reading, suppression(organizational), 1800, 0.75).
narrative_ontology:measurement(fla_hdr_grid_30, family_law_authority__hindu_dharmashastra_reading, suppression(organizational), 2024, 0.55).
narrative_ontology:measurement(fla_hdr_grid_31, family_law_authority__hindu_dharmashastra_reading, suppression(structural), 1800, 0.78).
narrative_ontology:measurement(fla_hdr_grid_32, family_law_authority__hindu_dharmashastra_reading, suppression(structural), 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% Family_law_authority kernel decomposes into five readings with distinct ε profiles: this reading (tangled_rope, ε=0.72) shows high extraction from identity-locked suppression of women/lower-castes; muslim_shariat_reading likely rope/tangled_rope with contractual protections (mehr, iddat) but gender asymmetry; christian_canonical_reading likely scaffold/tangled_rope with indissolubility but clerical annulment pathways; parsi_zoroastrian_reading likely rope with community coordination but demographic extraction; secular_contractual_reading likely rope with low extraction but state enforcement costs. The readings coexist as parallel personal law systems in India with state recognition, creating institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, powerful, 0.25).
constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, moderate, 0.82).
constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
