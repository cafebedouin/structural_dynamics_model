% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Judicial Reform of Personal Law on Gender Equality Grounds
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint story instantiates the gender_rights_reading of the
 *   marriage_authority kernel: the position that personal law systems
 *   (particularly Muslim personal law in India, but structurally
 *   generalizable) must be reformed through judicial expansion of
 *   constitutional equality guarantees to address intra-community gender
 *   inequality. The reading targets specific practices — triple talaq
 *   (instant divorce), maintenance denial, unequal property rights — rather
 *   than seeking wholesale replacement of personal law systems. It cross-cuts
 *   the communal/secular divide: secularists support it as a step toward
 *   Uniform Civil Code, communal autonomists oppose it as state encroachment,
 *   federalist millet proponents see it as undermining the pluralist compact,
 *   and judicial harmonization proponents view it as part of their
 *   case-by-case approach. The constraint is a snare from the structural seat
 *   of women_within_patriarchal_personal_law: they are extracted from by
 *   patriarchal practices AND by the reform's disruption of community
 *   protection structures, while the coordination story (gender equality) is
 *   real but the extraction is asymmetric and actively enforced through court
 *   orders that community authorities resist.
 *
 * KEY AGENTS:
 *   - women_rights_advocates: Primary beneficiary (institutional/mobile) — drives reform litigation, collects symbolic and material gains from legal victories
 *   - women_within_patriarchal_personal_law: Primary victim (powerless/identity_locked) — bears both patriarchal extraction and reform's collateral disruption to community support
 *   - constitutional_court: Agenda setter (institutional/generational) — authors the expanding equality jurisprudence, administers the constraint
 *   - traditional_community_authorities: Victim (organized/constrained) — loses interpretive monopoly over family law, bears enforcement costs of resistance
 *   - personal_law_boards: Victim (organized/constrained) — institutional face of communal autonomy, forced into defensive litigation
 *   - progressive_legal_scholars: Beneficiary (organized/biographical) — gains professional recognition and policy influence from reform jurisprudence
 *   - secularist_legislators: Observer (institutional/generational) — watches reform advance UCC agenda without legislative action
 *   - federalist_millet_proponents: Excluded (organized/biographical) — sees reform as majoritarian capture of pluralist compact, not in conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.78).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.72).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Judicial Reform of Personal Law on Gender Equality Grounds").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'e9a1357c-9f72-41fc-9f85-a1606b9450a1').
narrative_ontology:cs_kernel_codification('e9a1357c-9f72-41fc-9f85-a1606b9450a1', fixed_text).
narrative_ontology:cs_authority_grounding('e9a1357c-9f72-41fc-9f85-a1606b9450a1', lineage).
narrative_ontology:cs_interpretation_layer_present('e9a1357c-9f72-41fc-9f85-a1606b9450a1').
narrative_ontology:cs_reading_relation('e9a1357c-9f72-41fc-9f85-a1606b9450a1', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('e9a1357c-9f72-41fc-9f85-a1606b9450a1', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('e9a1357c-9f72-41fc-9f85-a1606b9450a1', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9a1357c-9f72-41fc-9f85-a1606b9450a1', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('e9a1357c-9f72-41fc-9f85-a1606b9450a1', foundational, constitutional_equality_overrides_personal_law).
narrative_ontology:cs_axiom_status(constitutional_equality_overrides_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('e9a1357c-9f72-41fc-9f85-a1606b9450a1', constitutional_equality_overrides_personal_law, conventional).
narrative_ontology:cs_axiom('e9a1357c-9f72-41fc-9f85-a1606b9450a1', foundational, gender_justice_as_non_derogable_fundamental_right).
narrative_ontology:cs_axiom_status(gender_justice_as_non_derogable_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('e9a1357c-9f72-41fc-9f85-a1606b9450a1', gender_justice_as_non_derogable_fundamental_right, deontological).
narrative_ontology:cs_axiom('e9a1357c-9f72-41fc-9f85-a1606b9450a1', secondary, judicial_reform_as_legitimate_substitute_for_legislative_inaction).
narrative_ontology:cs_axiom_status(judicial_reform_as_legitimate_substitute_for_legislative_inaction, holdable).
narrative_ontology:cs_axiom_grounding('e9a1357c-9f72-41fc-9f85-a1606b9450a1', judicial_reform_as_legitimate_substitute_for_legislative_inaction, instrumental).
narrative_ontology:cs_reference_frame('e9a1357c-9f72-41fc-9f85-a1606b9450a1', classical_personal_law_equilibrium).
narrative_ontology:cs_drift_state('e9a1357c-9f72-41fc-9f85-a1606b9450a1', contemporary_constitutional_morality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9a1357c-9f72-41fc-9f85-a1606b9450a1', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, constitutional_court).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, progressive_legal_scholars).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, traditional_community_authorities).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, personal_law_boards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, lawyers, and activists who litigate personal law reform cases. They gain professional recognition, funding, and policy influence from successful judgments. Their exit is mobile — they can shift to other rights campaigns if this constraint dissolves. They do not personally bear the community disruption costs of the reforms they win.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    institutional, generational, mobile, national).

% Women subject to patriarchal personal law practices (triple talaq, unequal maintenance, denied property). They bear the original patriarchal extraction AND the collateral disruption when judicial reform destabilizes community-based support systems (maintenance arbitration, property settlement within community, remarriage networks). Their exit is identity_locked: leaving the community means losing kinship, economic security, and social identity. They are often not the ones bringing cases — cases are brought by advocates or courts suo motu.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer).

% Supreme Court and High Courts expanding constitutional equality guarantees (Articles 14, 15, 21) into personal law domain. They author the constraint's operation through judgments, control the pace and scope of reform, and gain institutional legitimacy as guardians of constitutional morality. Their exit is arbitrage — they can pivot to other constitutional domains. They administer the enforcement machinery (contempt, monitoring committees).
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious leaders, qazis, community elders who historically interpreted and enforced personal law. They lose interpretive monopoly, face contempt proceedings for non-compliance, and bear political costs of resisting court orders. Their exit is constrained: they can adapt interpretations (some do) but cannot exit the community authority role without losing their position. They mobilize religious freedom arguments and political allies.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, traditional_community_authorities, payer,
    organized, biographical, constrained, local).

% Institutional bodies (e.g., All India Muslim Personal Law Board) that represent communal autonomy in legal/political forums. They bear litigation costs, face public delegitimization, and lose jurisdictional authority. Their exit is constrained: they exist to defend personal law; dissolving would concede the reform agenda. They negotiate compliance frameworks to retain residual authority.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, personal_law_boards, payer,
    organized, biographical, constrained, national).

% Academics and jurists who provide the doctrinal architecture for judicial reform. They gain citations, policy appointments, and intellectual authority from the expanding jurisprudence. Their exit is mobile — their expertise transfers to other constitutional domains. They are not directly subject to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, progressive_legal_scholars, beneficiary,
    organized, biographical, mobile, national).

% Political actors who support Uniform Civil Code but benefit from judicial reform advancing their agenda without legislative action. They observe the constraint's operation, occasionally intervene with legislation (e.g., Muslim Women Protection of Rights on Marriage Act 2019), but do not administer or directly bear its costs.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secularist_legislators, observer,
    institutional, generational, analytical, national).

% Scholars and minority-rights advocates who view legal pluralism as a consociational safeguard against majoritarianism. They argue judicial reform captures the pluralist compact for majoritarian moral projects. They are structurally excluded from the reform conversation — their framework (pluralism as anti-tyranny) is treated as obstructionist by both gender_rights and secularist readings.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, federalist_millet_proponents, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, constitutional_court).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the collective action problem of gender inequality within communities where women cannot individually negotiate equal rights because community norms, economic dependency, and identity fusion prevent exit. Judicial constitutional expansion provides an external lever to shift the equilibrium.
% TRANSFER_FUNCTION: Moves interpretive authority over family law from community authorities (qazis, personal law boards) to constitutional courts; moves material resources (maintenance, property) from husbands/families to women via court orders; moves political legitimacy from communal autonomy to constitutional morality.
% ABSENT_VOICES: Women who do not want judicial intervention — those who prefer community-based resolution, fear community ostracism from court cases, or view reform as threatening their negotiated position within the patriarchal bargain. Also excluded: federalist_millet_proponents who see the reform as majoritarian capture of the pluralist compact. These voices are not in the courtroom; the constraint's operation assumes they do not exist or are false consciousness.
% DISAPPEARANCE_RATIONALE: If the gender_rights_reading constraint vanished overnight: community authorities would regain interpretive monopoly over triple talaq, maintenance, and property; women_within_patriarchal_personal_law would lose constitutional-law remedies but retain community-based (unequal) systems; women_rights_advocates would shift to legislative UCC campaigns; constitutional_court would lose a major jurisprudential domain; secularist_legislators would lose judicial advance on their agenda. The personal law field would reorganize around communal_autonomy or federalist_millet frames.
% FOUNDING_PROBLEM: Gender inequality in personal law systems: women denied equal divorce rights, maintenance, inheritance, and property within community-governed family law. Communities internally blocked reform; legislatures avoided it for electoral reasons; constitutional guarantees existed but were not applied to personal law.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations (outside the beneficiary set of any single reading) document ongoing inequality: triple talaq persistence despite criminalization, maintenance awards below subsistence, daughters denied agricultural land inheritance. National Family Health Survey data shows Muslim women's property ownership and decision-making autonomy lag behind other groups. Law Commission reports (2018, 2022) confirm the problem persists. No community authority attests the problem is solved.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint operates through asymmetric transfer: community authorities lose interpretive control and enforcement power (extraction from them), while women_within_patriarchal_personal_law experience both the original patriarchal extraction AND the reform's disruption of community-based maintenance/property/support systems. Suppression is high (0.72) because the constraint's persistence depends on active judicial enforcement against community resistance — court orders, contempt proceedings, police registration of cases against community leaders. Theater ratio is moderate (0.38): the gender equality justification is genuine but a growing share of judicial energy goes to managing community backlash rather than delivering material outcomes to women. Accessibility collapse is moderate (0.52): alternatives exist (legislative UCC, community internal reform, private arbitration) but are structurally blocked by the same communal/secular polarization. Resistance is high (0.68): community authorities mobilize religious freedom arguments, political parties instrumentalize the issue, and implementation faces active non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional_court seat (agenda_setter, institutional, analytical exit), the constraint appears as genuine coordination: expanding constitutional guarantees to cover intimate life, solving the collective action problem of gender inequality within communities. From the women_within_patriarchal_personal_law seat (victim, powerless, identity_locked exit), the same structure operates as a snare: they are the object of reform rather than its subject, their community support networks are disrupted by court orders they did not seek, and their identity-locked exit means they cannot leave the community without losing their entire social world. From the traditional_community_authorities seat (victim, organized, constrained exit), the constraint is extraction of their jurisdictional authority enforced by a state they view as hostile. The engine computes this divergence from the structural data — the claimed snare type reflects the analyst seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Women_rights_advocates and constitutional_court are structural beneficiaries: they collect professional standing, jurisprudential authority, and policy influence from the reform (d near 0.0-0.2). Women_within_patriarchal_personal_law are structural targets: they bear the double extraction (patriarchal practices + reform disruption) with identity_locked exit (d near 0.8-0.9). Traditional_community_authorities and personal_law_boards are secondary targets: they lose interpretive monopoly and face enforcement costs, but have organized exit options through political mobilization (d near 0.6-0.7). Progressive_legal_scholars are incidental beneficiaries (d near 0.3). Secularist_legislators are observers with analytical exit (d = 0.5). Federalist_millet_proponents are excluded — their structural position is not captured by the constraint's operation but they would object if present.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by exposing the asymmetric extraction: the gender equality coordination function is real (women DO gain legal rights), but it operates through a structure that extracts from the very women it claims to benefit (disruption of community protection) and from community authorities (loss of jurisdiction) without their consent. The mandatrophy trap would be calling this a 'rope' (pure coordination) because women gain formal rights, or a 'tangled_rope' because community authorities also lose power. The snare classification captures that the coordination story is cover for a structure whose persistence depends on coercion (judicial enforcement against community resistance) and whose victims include the nominal beneficiaries. The founding problem (gender inequality in personal law) is live but the arrangement's extraction profile has intensified beyond the founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the gender_rights_reading a distinct constraint from the kernel''s other readings, or a measurement perspective on the same constraint?',
    'ε-invariance test: if the standing arrangement''s extractiveness differs structurally when assessed from the gender_rights_reading''s frame versus the communal_autonomy_reading''s frame, they are different constraints. The gender_rights_reading targets specific practices (triple talaq, maintenance, property) with women as victims of those practices, while communal_autonomy_reading targets state encroachment with community authorities as victims — different beneficiary/victim structures, different ε referents.',
    'If they are the same constraint, the kernel decomposition violates ε-invariance. If different, each reading instantiates its own constraint story with independent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or perspectival variants').

omega_variable(
    beneficiary_victim_ambiguity_women,
    'Are women_within_patriarchal_personal_law beneficiaries of the judicial reform (as the reading claims) or victims of the reform''s disruption of their community''s protective structures?',
    'Longitudinal study of women''s material outcomes post-reform: maintenance awards, property access, community support networks, remarriage prospects. Compare jurisdictions with and without such judicial interventions.',
    'If women are net beneficiaries, the constraint is a tangled_rope (coordination + asymmetric extraction from community authorities). If women are net victims of reform disruption, the constraint is a snare (extraction from women via both patriarchal practices AND reform''s collateral damage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_ambiguity_women, empirical, 'Whether the intended beneficiaries of gender-equality reform are also its victims').

omega_variable(
    enforcement_capacity_gap,
    'Does the judicial expansion of constitutional guarantees have actual enforcement capacity at the local level, or does it create a performative rights layer while patriarchal practices continue unchanged?',
    'Track implementation metrics: court orders compliance rates, police registration of cases, community-level dispute resolution outcomes, legal aid accessibility for affected women.',
    'If enforcement is performative (high theater_ratio, low actual behavior change), the constraint is a piton or scaffold. If enforcement is effective but contested, it remains a snare/tangled_rope with active suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Whether judicial reform translates to on-the-ground change or remains symbolic').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, community enforcement, economic dependency) or internalized (women believing reform threatens their security, identity fusion with community norms)?',
    'Post-reform suppression trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized. Qualitative interviews with women who accessed reform vs. those who did not.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after legal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/communal constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1985, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority__gender_rights_reading_tr_t1985, marriage_authority__gender_rights_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_tr_t1995, marriage_authority__gender_rights_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_tr_t2005, marriage_authority__gender_rights_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_tr_t2015, marriage_authority__gender_rights_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_tr_t2023, marriage_authority__gender_rights_reading, theater_ratio, 2023, 0.38).

% Extraction over time
narrative_ontology:measurement(marriage_authority__gender_rights_reading_be_t1985, marriage_authority__gender_rights_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_be_t1995, marriage_authority__gender_rights_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_be_t2005, marriage_authority__gender_rights_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_be_t2015, marriage_authority__gender_rights_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_be_t2023, marriage_authority__gender_rights_reading, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority__gender_rights_reading_su_t1985, marriage_authority__gender_rights_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_su_t1995, marriage_authority__gender_rights_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_su_t2005, marriage_authority__gender_rights_reading, suppression_requirement, 2005, 0.61).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_su_t2015, marriage_authority__gender_rights_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(marriage_authority__gender_rights_reading_su_t2023, marriage_authority__gender_rights_reading, suppression_requirement, 2023, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: marriage_authority kernel has 5 readings. This reading (gender_rights) targets specific practices via judicial constitutional expansion; communal_autonomy defends community interpretive monopoly; federalist_millet defends pluralism as structural anti-majoritarianism; judicial_harmonization pursues case-by-case constitutional floor; secularist pursues legislative UCC. The gender_rights and judicial_harmonization readings share the judicial instrument but differ in scope (targeted practices vs. systemic floor) and beneficiary structure (women_rights_advocates vs. constitutional_court as primary beneficiary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, powerless, 0.85).
constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, organized, 0.65).
constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
