% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy: Divine Right and Hereditary Succession
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This constraint embodies the legitimacy claim of monarchical authority:
 *   that political power flows downward from a sovereign whose claim rests on
 *   divine sanction, inherited right through bloodline, and the necessity of
 *   hierarchical order. The reading asserts that this descent is not
 *   constructed but natural law — divinely ordained and historically
 *   necessary. However, structural analysis reveals high extraction from
 *   excluded subjects and active suppression of alternative legitimacy
 *   frames. The constraint performs genuine coordination (it does prevent
 *   succession chaos through clear rules) while simultaneously extracting
 *   obedience, wealth, and foreclosed agency from those it rules. The
 *   divergence between the claimed naturalness and the measured
 *   extractiveness is precisely the phenomenon the framework detects: a false
 *   summit where identifiable beneficiaries (the ruling class) profit from
 *   treating a constructed arrangement as inevitable.
 *
 * KEY AGENTS:
 *   - Sovereign Monarch: sets law, claims divine sanction, bears succession crisis risk (institutional power, trapped exit)
 *   - Hereditary Ruling Class: collects privilege and subordinate authority, maintains local enforcement (powerful, identity-locked to the hierarchy)
 *   - Ecclesiastical Authority: grants divine validation, receives land and monopoly (institutional, constrained by mutual dependence)
 *   - Excluded Subjects: bear the constraint's full extractive weight without voice or consent (powerless, trapped)
 *   - Alternative Legitimacy Claimants: actively suppressed; their exclusion is the constraint's enforcement object (moderate, constrained by suppression)
 *   - Tradition Keepers: historians and clergy maintaining the narrative (organized, analytical seat)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.81).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.87).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy: Divine Right and Hereditary Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '3ad83234-0522-4443-aa29-f9df34e45879').
narrative_ontology:cs_kernel_codification('3ad83234-0522-4443-aa29-f9df34e45879', fixed_text).
narrative_ontology:cs_authority_grounding('3ad83234-0522-4443-aa29-f9df34e45879', lineage).
narrative_ontology:cs_interpretation_layer_present('3ad83234-0522-4443-aa29-f9df34e45879').
narrative_ontology:cs_reading_relation('3ad83234-0522-4443-aa29-f9df34e45879', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('3ad83234-0522-4443-aa29-f9df34e45879', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3ad83234-0522-4443-aa29-f9df34e45879', foundational, authority_descends_divinely).
narrative_ontology:cs_axiom_status(authority_descends_divinely, holdable).
narrative_ontology:cs_axiom_grounding('3ad83234-0522-4443-aa29-f9df34e45879', authority_descends_divinely, theological).
narrative_ontology:cs_axiom('3ad83234-0522-4443-aa29-f9df34e45879', foundational, bloodline_continuity_legitimacy).
narrative_ontology:cs_axiom_status(bloodline_continuity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3ad83234-0522-4443-aa29-f9df34e45879', bloodline_continuity_legitimacy, conventional).
narrative_ontology:cs_axiom('3ad83234-0522-4443-aa29-f9df34e45879', secondary, hierarchy_natural_necessity).
narrative_ontology:cs_axiom_status(hierarchy_natural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3ad83234-0522-4443-aa29-f9df34e45879', hierarchy_natural_necessity, deontological).
narrative_ontology:cs_reference_frame('3ad83234-0522-4443-aa29-f9df34e45879', divinely_ordained_succession_hierarchy).
narrative_ontology:cs_drift_state('3ad83234-0522-4443-aa29-f9df34e45879', enlightenment_and_democratic_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3ad83234-0522-4443-aa29-f9df34e45879', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises authority claimed as inherited directly from divinity, mediated through bloodline continuity. Justifies rule by reference to sacred succession, divine sanction, and the necessity of hierarchical order. Sets all significant law and policy. Bears the legitimacy burden of succession crises and contested claims to the throne, which threaten the entire framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, sovereign_monarch, agenda_setter,
    institutional, generational, trapped, national).

% Derives privilege, wealth, and power from the monarchical order. Holds subordinate but secure positions in the hierarchy, with their own inheritance and subordinate authority flowing downward from the crown. Their identity, social position, and material interest are fused with the monarchical framework — exiting would mean renouncing aristocratic status and estate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, beneficiary,
    powerful, generational, identity_locked, national).

% Administers subordinate authority and collects rents from feudal relationships and tax privileges. They maintain the constraint by enforcing the sovereign's will locally and ritually performing their subordination. They also bear costs: they can be dispossessed by sovereign will, and they are locked into performing loyalty and submission continuously.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, payer).

% Bear the constraints' full weight: they are subject to law they did not consent to, pay taxes and levies set without their participation, and are excluded from any legitimacy claim. Their exit options are literal: flight, rebellion (which is treason), or death. The constraint's suppression falls most heavily here — the legitimacy claim explicitly denies them standing to question authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_subjects, payer,
    powerless, biographical, trapped, national).

% Any party (rival noble house, religious authority, popular movement, commercial elite) that asserts a competing legitimacy claim is immediately positioned as traitorous or seditious. The constraint's enforcement machinery actively suppresses their voice: censorship, excommunication, imprisonment, or execution. Their material cost is the risk of violent reprisal; their structural cost is that their legitimacy frame cannot be spoken in public without triggering enforcement.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants, payer,
    moderate, biographical, constrained, national).

% Grants divine sanction to the monarch (coronation, blessing, doctrine of divine right) and receives land, wealth, tax exemption, and monopoly on certain forms of authority (marriage, moral judgment) in return. The church's legitimacy becomes entangled with the crown's — if the crown falls, the church's blessing becomes worthless; if the church withdraws sanction, the monarch's claim weakens. Both are locked into the performance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, ecclesiastical_authority, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, ecclesiastical_authority, payer).

% Secondary heirs, cadet branches, or disputed claimants have a material interest in the succession rules but no legitimate voice until the throne is actually contested. They are structurally bound to accept the current succession order or mount armed rebellion. Even close family members are excluded from co-authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, succession_line_pretenders, excluded,
    moderate, biographical, trapped, national).

% Intellectual and political movements asserting the people as the legitimate source of authority are explicitly ruled out by this reading's core premise. They are excluded from the legitimate conversation and are treated as sedition if they gain public voice. They would reconstruct the entire legitimacy framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, popular_sovereignty_advocates, excluded,
    moderate, biographical, constrained, national).

% Historians, clergy, and court officials who maintain the narrative of bloodline continuity, divine sanction, and the necessity of hierarchy. Their interpretive work sustains the legitimacy claim by keeping the historical record aligned with the reading's premises. They have no direct power but are essential to the constraint's persistence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, tradition_keepers, observer,
    organized, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single decision-making center with clear succession rules to prevent political fragmentation and succession wars. Provides a known authority structure and unified command for the realm. Creates a hierarchy of delegated authority that does not require constant renegotiation.
% TRANSFER_FUNCTION: Moves obedience, tribute, military service, and legal compliance from subjects upward to the sovereign; moves legitimacy, protection, and order downward from sovereign through the hierarchy. Also transfers wealth and privilege to the ruling class through estate, title, and tax immunity.
% ABSENT_VOICES: Popular assemblies, democratic movements, republican advocates, and women (in most monarchical readings) are structurally excluded. Merchants and rising commercial classes are absent from the legitimacy conversation despite accumulating economic power. Religious minorities that deny the crown's divine sanction are silenced.
% DISAPPEARANCE_RATIONALE: If divine-right monarchical legitimacy disappeared overnight, the realm would lose its sole recognized authority frame. Succession crises would multiply, rival claimants would assert competing legitimacy grounds (popular consent, contractual governance, alternative religious interpretation), and the hierarchy of inherited privilege would collapse without its sacred foundation. The political order would reorganize around entirely different legitimacy principles — this constraint is not a natural law but a constructed arrangement whose removal unmakes the system it grounds.
% FOUNDING_PROBLEM: In the absence of a clear, divinely-sanctioned authority structure, realms fractured into civil war, local warlords competed for power, and no unified law could be established. A hereditary succession rule, grounded in divine right and backed by ritual and doctrine, solved the succession crisis by making legitimacy questions illegitimate — the sovereign's bloodline IS the answer.
% FOUNDING_PROBLEM_CORROBORATION: Modern scholarship and contemporary political actors outside the monarchical frame attest that succession disputes in pre-monarchical polities were real; however, they also attest that the divine-right solution was not the only possible answer and that succession mechanisms exist (constitutional succession, electoral procedures) that prevent war without requiring monarchical authority. Legislative bodies and democratic movements founded explicitly to contest this reading provide external corroboration that the founding problem no longer requires this solution.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.81) is high because the arrangement transfers obedience and wealth from subjects to ruler without reciprocal consent or market exchange. The extraction is legitimized through divine sanction language, making it appear natural rather than coercive. Suppression (0.87) is very high because the constraint's persistence depends not on voluntary acceptance but on active silencing of rival legitimacy frames — this is tangled rope because the coordination function (succession rules preventing chaos) is genuine and valued by beneficiaries, but it coexists with substantial asymmetric extraction from excluded subjects. Theater ratio (0.42) is moderate-to-high and rising: the divine-right ritual (coronation, ceremonial performance, bloodline pageantry) is real and carries cultural meaning, but increasingly these performances sustain the legitimacy claim more than practical governance function, especially as the constraint is challenged. The measurement series shows rising extractiveness and suppression over the interval: as the authority becomes more contested (by enlightenment thought, democratic movements, commercial power), the enforcement machinery intensifies and the theatrical work required to maintain legitimacy increases. This is precisely the pattern of a constraint becoming a piton — the function has atrophied (replaced by constitutional mechanisms in many contexts) but the constraint persists through increasingly performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign seat sees a natural, divinely-mandated order that solves succession. The subject seat sees brutal extraction. The beneficiary seats (aristocracy, clergy) see legitimate privilege. The alternative legitimacy seats see censored voices and foreclosed frames. The engine's per-seat computation surfaces these structural divergences without reconciling them — that is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the sovereign and hereditary ruling class (d near 0.0–0.2): they collect legitimacy validation, privilege, wealth, and power from the arrangement and have strong incentives to maintain it. Ecclesiastical authority is also a beneficiary (d near 0.1–0.3) because they profit from granting sanction, though they are constrained by mutual dependence. Victims are excluded subjects (d near 0.9–1.0): they bear obedience, taxation, legal powerlessness, and foreclosed voice with no reciprocal benefit. Alternative legitimacy claimants are also targets (d near 0.8–1.0): they are actively suppressed and risk their lives or liberty if they speak their frame publicly. The aristocratic hierarchy occupies a mixed position (d near 0.4–0.6): they benefit from inherited privilege but are also locked into performing submission to the sovereign and are vulnerable to arbitrary dispossession. Their identity_locked exit reflects that renouncing aristocratic status would be renouncing their entire social being.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of a false summit. It is claimed as natural law (divine right, bloodline necessity, natural hierarchy) but is structured as tangled rope: it performs genuine coordination (succession rules) while extracting asymmetrically from subjects and suppressing rival frames. The mandatrophy resolution lies in the founding problem status: the founding problem (succession wars in pre-monarchical polities) is DEAD in modern contexts where constitutional succession mechanisms and democratic procedures prevent the same outcome without divine right. Yet the constraint persists — the measurement series shows rising theater_ratio and persistent suppression as the functional justification decays. The constraint has become a piton: the beneficiaries (ruling class, ecclesiastical authority) maintain it through ritual and doctrine even though the coordination function has been solved by alternative mechanisms. The rising suppression requirement (from 0.72 to 0.87) reflects that modern opposition forces are stronger and the legitimacy claim requires more active enforcement. This is mandatrophy in progress: the mandate is obsolete but the machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is divine sanction a genuine metaphysical claim about authority''s origin, or a post-hoc legitimacy narrative grafted onto an inherited power structure?',
    'Comparative analysis of succession disputes in contexts where divine sanction claims have been withdrawn (e.g., post-monarchical transitions); examination of whether replacement legitimacy frames (constitutional law, popular consent) prevent chaos as effectively without divine claims.',
    'If divine sanction is merely narrative cover, the constraint is snare-classified (pure extraction with a coordination cover story). If divine sanction has genuine constitutive force within the reading''s framework, the constraint remains tangled rope (genuine coordination + extraction). The resolution does not change the structural metrics but changes interpretation: is the divinity claim epistemic (true/false) or performative (legitimate within the frame)?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'Whether divine sanction is metaphysical or narrative legitimation').

omega_variable(
    succession_rule_necessity,
    'Is hereditary succession the ONLY mechanism that prevents succession chaos, or do other rule sets (electoral succession, constitutional designation, lottery rotation) achieve the same coordination at lower extraction cost?',
    'Historical and comparative study of non-monarchical succession systems (constitutional monarchy, elective monarchy, republican succession) and their succession-stability records.',
    'If alternatives exist and achieve comparable stability, hereditary succession is a contingent choice favoring the ruling class''s interests, not a necessary coordination mechanism — the constraint reclassifies toward snare. If alternatives fail to prevent chaos, the constraint''s coordination claim is validated and remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_rule_necessity, empirical, 'Whether hereditary succession is uniquely necessary for succession stability').

omega_variable(
    suppression_structurality,
    'Is the suppression of alternative legitimacy claims structurally necessary for the monarchical frame (i.e., the frame cannot coexist with live alternative frames), or is it a choice to maximize extraction and stability at the cost of voice?',
    'Examination of historical monarchies that have tolerated intellectual opposition (e.g., Enlightenment monarchies with censored-but-visible dissent) versus those that have crushed all opposition; analysis of whether toleration weakens or strengthens the constraint.',
    'If suppression is necessary, the constraint is tangled rope (coordination + forced extraction). If suppression is optional, it is snare (the coordination cover is thin and the machinery is primarily extractive). The structural answer affects how alternative readings would be treated: in tangled rope, they are real threats; in snare, they are merely suppressed competitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structurality, empirical, 'Whether suppression is structurally necessary or strategically chosen').

omega_variable(
    identity_fusion_reversibility,
    'For the aristocratic hierarchy and ecclesiastical authority, is the identity-fusion with monarchical legitimacy permanent (they cannot conceive of selfhood outside the frame), or strategically maintained (they choose to fuse because the benefits are high)?',
    'Empirical study of transition moments (revolutions, regime changes) where high-status beneficiaries renounce the old frame and adopt new ones, and of cases where they resist transition: if they transition readily when incentivized, identity_locked is strategic; if transition is psychologically destabilizing, fusion is real.',
    'If fusion is strategic, the beneficiary seats have higher directionality d toward target than authored (they could exit but choose not to, making them complicit); if fusion is real, d remains low (they truly cannot see themselves outside the frame). The distinction affects how beneficiary exit is modeled: strategic exit is always possible; fused exit requires cognitive reorientation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_reversibility, empirical, 'Whether identity-fusion of the ruling class is permanent or strategically maintained').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the sovereign_legitimacy kernel. Sibling readings (republican_reading, constitutional_hybrid_reading) propose fundamentally different legitimacy grounds. Are these readings genuinely incommensurable (no single framework could hold more than one), or do they coexist as live positions held by different factions within the same institutional space?',
    'Historical examination of transition moments and contested kingdoms where multiple readings were simultaneously held and performed by different parties; analysis of whether the readings are logically foreclosing or politically coexisting.',
    'If the readings foreclose each other, only one can be true and political change is a transition between incompatible worlds. If the readings coexist, they are competing frames that can be held simultaneously by different parties and the constraint''s classification depends on the distribution: monarchical dominance in one region, republican ascendance in another, or constitutional compromise bridging both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the three readings of sovereign_legitimacy foreclose or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovereign_monarchical_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sovereign_monarchical_tr_t5, sovereign_legitimacy__monarchical_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(sovereign_monarchical_tr_t10, sovereign_legitimacy__monarchical_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(sovereign_monarchical_tr_t15, sovereign_legitimacy__monarchical_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(sovereign_monarchical_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(sovereign_monarchical_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(sovereign_monarchical_tr_t30, sovereign_legitimacy__monarchical_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(sovereign_monarchical_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sovereign_monarchical_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sovereign_monarchical_be_t5, sovereign_legitimacy__monarchical_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(sovereign_monarchical_be_t10, sovereign_legitimacy__monarchical_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(sovereign_monarchical_be_t15, sovereign_legitimacy__monarchical_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(sovereign_monarchical_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(sovereign_monarchical_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(sovereign_monarchical_be_t30, sovereign_legitimacy__monarchical_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(sovereign_monarchical_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(sovereign_monarchical_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(sovereign_monarchical_su_t5, sovereign_legitimacy__monarchical_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(sovereign_monarchical_su_t10, sovereign_legitimacy__monarchical_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(sovereign_monarchical_su_t15, sovereign_legitimacy__monarchical_reading, suppression_requirement, 15, 0.81).
narrative_ontology:measurement(sovereign_monarchical_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(sovereign_monarchical_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement(sovereign_monarchical_su_t30, sovereign_legitimacy__monarchical_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(sovereign_monarchical_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, aristocratic_privilege_enforcement).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, divine_right_doctrine_vindication).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, succession_law_naturalism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sovereign_legitimacy kernel. The sibling readings (republican_reading, constitutional_hybrid_reading) instantiate fundamentally different beneficiary/victim structures and legitimacy grounds from the same contested kernel. They are NOT variants of the same constraint but separate constraints with different ε values, suppression profiles, and classifications. Decomposition follows the ε-invariance principle: the legitimate authority claim itself is the kernel; the reading (monarchical vs. republican vs. constitutional) determines which structural facts count as evidence and which actors count as beneficiaries/victims. All three readings share the kernel's core question but produce incompatible constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
