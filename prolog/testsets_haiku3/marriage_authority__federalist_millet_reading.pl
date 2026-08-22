% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Fragmented Marriage Authority as Federalist Anti-Tyranny Mechanism
 *   domain: legal/constitutional/comparative_family_law
 *
 * SUMMARY:
 *   This constraint instantiates the federalist-millet reading of marriage
 *   authority fragmentation: the deliberate constitutional choice to allocate
 *   family law jurisdiction to multiple religious and customary legal systems
 *   rather than a unified secular code, justified as a mechanism to prevent
 *   majoritarian tyranny in a multinational state. The reading emphasizes the
 *   anti-tyranny function and treats legislative paralysis on family law as a
 *   feature (institutional protection) rather than a bug (gridlock). This
 *   reading overlaps structurally with the communal-autonomy reading (both
 *   defend minority authority) but differs in its foundational justification:
 *   this reading grounds authority fragmentation in anti-majoritarian
 *   federalism and elite constitutional bargaining, not in the intrinsic
 *   religious authority of communities. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as rope (genuine coordination problem
 *   solved—majoritarian tyranny prevention) while the authored metrics show
 *   moderate extractiveness (0.28 at interval end) because the fragmentation
 *   also entrenches intra-community gender inequality and excludes women from
 *   the political bargain. The engine will compute per-seat types, revealing
 *   whether the beneficiary seats (minority communities) and payer seats
 *   (secular reformers, excluded women) experience it differently.
 *
 * KEY AGENTS:
 *   - religious_minority_communities — primary beneficiaries of authority fragmentation; constrained exit to protected family law governance
 *   - secular_legal_reformers — payers (transactionally costlier reform path); institutional power but politically constrained by anti-tyranny doctrine
 *   - women_within_minority_communities — excluded from the bargaining table; identity-locked; structural losers despite majority-group gains in secular family law
 *   - legislative_majority — agenda-setter with formal supremacy but constrained practical authority; maintains legitimacy by exercising restraint
 *   - constitutional_court — observer seat; enforces constitutional floors without dismantling pluralism; caught between anti-tyranny and equality mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Fragmented Marriage Authority as Federalist Anti-Tyranny Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/comparative_family_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965').
narrative_ontology:cs_kernel_codification('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', formalized).
narrative_ontology:cs_authority_grounding('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', lineage).
narrative_ontology:cs_interpretation_layer_present('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965').
narrative_ontology:cs_reading_relation('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', marriage_authority__gender_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_axiom('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', foundational, majoritarian_tyranny_prevention_is_superior_to_unified_equality).
narrative_ontology:cs_axiom_status(majoritarian_tyranny_prevention_is_superior_to_unified_equality, holdable).
narrative_ontology:cs_axiom_grounding('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', majoritarian_tyranny_prevention_is_superior_to_unified_equality, deontological).
narrative_ontology:cs_axiom('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', foundational, constitutional_fragmentation_of_family_law_is_intrinsic_to_multinational_legitimacy).
narrative_ontology:cs_axiom_status(constitutional_fragmentation_of_family_law_is_intrinsic_to_multinational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', constitutional_fragmentation_of_family_law_is_intrinsic_to_multinational_legitimacy, conventional).
narrative_ontology:cs_reference_frame('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', anti_majoritarian_federalism).
narrative_ontology:cs_drift_state('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', contemporary_gender_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c5c3b0e-1aa2-4d1f-a375-6d38a37c3965', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, cultural_autonomy_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, secular_legal_reformers).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_democracy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, pluralist_stability_hypothesis).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, majoritarian_tyranny_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain internal authority over marriage, divorce, and family succession within their religious laws (Islamic Shariat, Hindu personal law, Christian marriage canon where applicable). The fragmented authority structure protects their ability to govern family life by their own norms without state-mandated uniformity. Exit would mean abandoning community identity and religious practice tied to marriage jurisdiction.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, religious_minority_communities, beneficiary,
    organized, generational, constrained, national).

% Access to personal law systems that reflect their community's historical norms and values rather than a secular state norm imposed uniformly. The fragmentation prevents cultural homogenization by legislative majority. Exit means assimilation into majoritarian legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, cultural_autonomy_constituencies, beneficiary,
    organized, generational, constrained, national).

% Holds formal legislative authority over law but is structurally prevented from using it to impose a uniform civil code or override personal law regimes. The fragmentation creates legislative paralysis on family law—they cannot unify without triggering majoritarian-tyranny concerns. They maintain formal supremacy while exercising constrained practical authority.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, legislative_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek to enact a Uniform Civil Code but face structural resistance: the fragmentation makes legislative action on family law politically costlier because any unification violates the anti-tyranny principle and triggers minority backlash. They must win explicit consent across communities rather than impose majoritarian will. The constraint raises the transaction cost of their preferred reform.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_legal_reformers, payer,
    institutional, generational, mobile, national).

% Are formally outside the legislative conversation on family law reform. Their interests in gender equality within family law are subordinated to the consociational bargain protecting community autonomy. They cannot exit without severing ties to community, religious identity, and often economic support.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_within_minority_communities, excluded,
    powerless, biographical, identity_locked, national).

% Reviews individual cases and occasionally imposes constitutional floor constraints (gender equality, due process) without formally dismantling personal law pluralism. Operates within the constraint rather than enforcing its removal. Analyzes whether the fragmentation achieves its stated anti-tyranny function or merely protects discriminatory practices.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Historical constitutional designers who established the personal law pluralism model to prevent majoritarian tyranny. Their original intent is invoked by defenders of the fragmentation and questioned by reformers seeking unification.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, federation_architects, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, religious_minority_communities).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the foundational consociational coordination problem: how to prevent majoritarian imposition of one cultural/religious family law norm on religious minorities in a multinational state, while maintaining overall legal order. The solution is deliberate authority fragmentation—each religious/cultural community retains jurisdiction over its own family law, backed by recognition in the constitution or statute.
% TRANSFER_FUNCTION: Allocates normative authority over marriage, divorce, succession, and guardianship from a unified secular legislature to multiple decentralized religious and customary legal systems. Majority legislative power is constrained (cannot unify family law without explicit opt-in from protected communities); minority communities gain stable control over their internal family governance. The transfer is authority, not material resources.
% ABSENT_VOICES: Women within minority communities who seek reform of discriminatory practices within personal law systems are structurally excluded—their interests are subordinated to the collective community autonomy protection. LGBTQ+ persons and those seeking to marry across religious lines are also excluded, as the system assumes marriages occur within community boundaries. Those who oppose religious family law on secular equality grounds are sidelined in the bargaining table.
% DISAPPEARANCE_RATIONALE: If fragmented marriage authority disappeared and a uniform civil code were imposed overnight, religious minority communities would lose internal governing authority over family law, triggering claims of majoritarian tyranny and potential destabilization of the multinational state's consociational settlement. Constitutional legitimacy would shift from protection-of-minorities to majoritarian-legal-uniformity, altering the entire federal bargain.
% FOUNDING_PROBLEM: How to structure family law in a multinational state with multiple religious and cultural communities such that the majoritarian group does not use legislative supremacy to impose its family law norms on minorities, thereby eroding cultural autonomy and triggering alienation or secession.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and federalism theorists attest the founding problem was live at founding (multinational state composition, colonial religious divides, need for constitutional legitimacy across groups). Minority community leaders attest it remains live—uniform civil code efforts are experienced as majoritarian encroachment. Secular legal reformers and gender-equality advocates attest the problem has shifted: the live question is now whether majoritarian tyranny is plausibly checked by a system that equally tolerates intra-community gender discrimination. Constitutional courts' equivocation (imposing equality floors without dismantling pluralism) reflects contested status.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the constraint genuinely solves the stated coordination problem (preventing majoritarian tyranny in a multinational state) and generates real benefits for minority communities (retained authority, stable cultural governance). But it is not zero because the fragmentation imposes costs on secular reformers (paralyzed legislatures, high transaction costs for unified reform) and, critically, on women and LGBTQ+ persons within minorities (intra-community discrimination entrenched by the pluralism-protection logic). The measurement series shows gradual increase in extractiveness over the interval (0.18 to 0.28) reflecting the growing salience of excluded-party interests (gender equality, sexual orientation rights, interfaith marriage) that the original consociational bargain did not contemplate. Theater ratio is very low (0.12 at end) because the fragmentation operates as stated—there is minimal pretense, just political negotiation. Suppression is also low (0.15) because the constraint operates largely through authority delegation rather than coercive exclusion; minority communities choose to govern family law, rather than being forced out. The modest levels reflect that this is genuinely a rope-type coordination, not masked snare, but one whose beneficiaries and payers are increasingly asymmetric as demographic and rights consciousness shift.
 *
 * PERSPECTIVAL GAP:
 *   The federalist-millet reading produces radically different type classifications depending on the seat. From the minority-community beneficiary seat: this is a genuine rope—it solves the coordination problem (preventing majoritarian tyranny) at acceptable cost, with the benefits of internal authority worth the price of not unifying. From the secular-reformer seat: the same structure is a snare—formal majoritarian supremacy is illusory, the paralysis is real, and the transaction costs of reform (needing explicit community consent) are extractive overhead, not coordination cost. From the women-within-minorities seat: it is a tangled rope or snare—they benefit from community autonomy in theory (they are members) but are excluded from the bargaining and bear hidden costs (entrenched family law discrimination, no voice in reform). The engine computes each seat's classification from the structural data; the perspectival gap is the difference between the federalist reading's intended beneficiaries (minorities protected from majoritarian tyranny) and its actual beneficiary structure (communities whose leadership controls family law, primarily through patriarchal governance).
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities have d near the beneficiary end (0.15–0.25 range): they gain authority retention and cultural stability; their exit options are constrained but not trapped (they remain organized and can negotiate). Secular reformers have d near the middle-to-target end (0.55–0.70): they possess institutional power but face legislative paralysis and high transaction costs; they are not victimized but are structurally impeded. Women within minorities have d at the target end (0.75–0.85): they are powerless, identity-locked (cannot exit without severing community ties), and excluded from the constitutional bargain; the constraint's benefits flow to community authority-holders, not to them. The legislative majority sits near symmetric (d ~0.5): they maintain formal supremacy but exercise restrained authority; they benefit from legitimacy-via-restraint but lose practical legislative control over family law. An override is unnecessary here—the structural derivation from beneficiary/victim declarations + exit options produces the right d values. The fragmentation structure naturally differentiates directionality across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (majoritarian tyranny prevention via legal pluralism) is live within the federalist reading. The constraint still prevents majoritarian imposition of a unified secular code, and minority communities still experience the benefit of retained authority. However, the reading's framing conceals a second-order problem: the fragmentation protects minorities from external (majoritarian) tyranny while entrenching internal (patriarchal, religious-authority) tyranny over women. The mandate of 'anti-majoritarian federalism' does not speak to this internal asymmetry. A mandatrophy analysis would surface whether the original founding problem (national unity without cultural erasure) has been solved well enough that the remaining function is now mostly maintaining vested authority-holder interests within communities, at the cost of excluding women. The measurement series shows extractiveness stabilizing around 0.28 (plateau after year 45), suggesting the constraint is no longer drifting—it has reached a quasi-equilibrium where the anti-tyranny function is locked in place alongside the internal discrimination it permits. This quasi-equilibrium is the mandatrophy signal: the constraint persists not because the founding problem is live, but because it has become self-maintaining through institutional inertia and narrative consensus (the anti-tyranny framing is hegemonic among elites). Genuine mandatrophy resolution would require either (a) the founding problem (majoritarian cultural erosion) to be authoritatively declared dead or solved, triggering a deliberate constitutional reset, or (b) a counter-narrative (gender equality within pluralism, or secular voluntarism) to dislodge the anti-tyranny framing from hegemonic status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_tyranny_vs_internalized_discrimination,
    'Does the fragmented authority structure genuinely prevent majoritarian tyranny, or does it merely substitute majoritarian tyranny (legislative imposition) with internalized minority tyranny (patriarchal family law enforced by community leadership)?',
    'Comparative analysis of gender equality outcomes in fragmented vs. unified family law systems; survey evidence on whether minority women experience the authority structure as protection or as entrapment; deliberative forums where excluded voices articulate their interests.',
    'If internalized discrimination is as severe as majoritarian imposition would be, the anti-tyranny framing is revealed as incomplete—the constraint protects minorities from external threat while ignoring internal domination. The classification would shift from rope (genuine coordination) toward tangled_rope or snare (extractive cover). If majoritarian imposition would be more severe, the rope classification is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_vs_internalized_discrimination, empirical, 'Whether fragmentation solves the stated problem or substitutes one tyranny for another.').

omega_variable(
    consociational_settlement_durability,
    'Is the elite constitutional bargain that created fragmented authority still binding on subsequent generations, or has it become a contingent institutional arrangement sustained by path-dependence and intellectual hegemony (the anti-tyranny narrative)?',
    'Generational attitude surveys on whether new cohorts accept the anti-tyranny justification; instances of attempted constitutional reform and their political outcomes; statements by younger community leaders on whether the bargain serves their interests.',
    'If the bargain is binding (genuine intergenerational contract), the constraint is a true rope with durable coordination value. If it is contingent (hegemonic narrative preventing recontracting), it drifts toward piton—maintained by institutional inertia and theoretical consensus, not by actual ongoing coordination benefit. The finding would sharpen the mandatrophy diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consociational_settlement_durability, empirical, 'Whether the consociational settlement is a durable elite bargain or a hegemonic narrative.').

omega_variable(
    exit_option_quality_for_minorities,
    'What is the real exit cost for a religious minority community that wishes to opt out of the fragmented authority structure and adopt a secular family law? Is the exit ''constrained'' (difficult but possible) or ''trapped'' (institutionally impossible without community dissolution)?',
    'Case studies of communities that have attempted to opt out; analysis of whether state law provides opt-out pathways; interviews with community members on whether exit is theoretically possible but practically unthinkable (identity-locked equivalent for a collective).',
    'If exit is trapped, the beneficiary directionality claim is overstated—minorities are not choosing pluralism, they are locked into it. The classification would shift toward snare. If exit is genuinely constrained (difficult but possible), the rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_quality_for_minorities, empirical, 'Whether minority communities can realistically exit the fragmented authority structure.').

omega_variable(
    secular_reformer_legislative_capacity_asymmetry,
    'Is the legislative paralysis on family law reform (secular reformers'' high transaction cost) a structural feature of the anti-tyranny design, or a contingent outcome of current political fragmentation?',
    'Comparative constitutional law analysis of other multinational federations and their family law reform procedures; historical analysis of whether earlier constitutional periods saw family law reform despite the anti-tyranny clause; counterfactual on what would change if secular reformers achieved legislative supermajority while respecting the anti-tyranny constraint.',
    'If paralysis is structural (anti-tyranny logic inherently requires supramajority consent for family law change), it is a true coordination cost. If contingent (could be reformed via constitutional amendment that preserves minority protections differently), it is extractive overhead. The distinction matters for rope vs. tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_reformer_legislative_capacity_asymmetry, conceptual, 'Whether legislative paralysis is intrinsic to anti-tyranny federalism or a contingent institutional choice.').

omega_variable(
    committer_underspecification_sibling_reading_pressure,
    'The federalist reading''s core claim (fragmentation prevents majoritarian tyranny) is also present, with different emphasis, in the communal_autonomy reading. Does the federalist reading''s additional justification (anti-tyranny as a constitutional design principle) meaningfully differentiate it, or is the difference merely rhetorical?',
    'Textual analysis comparing communal-autonomy defenses (grounded in religious authority as intrinsically legitimate) with federalist-millet defenses (grounded in constitutional anti-majoritarian architecture). Examine whether the two readings forecast different reform trajectories or institutional responses.',
    'If the difference is meaningful (federalist reading permits or encourages constitutional amendment that preserves minority protection differently; communal reading does not), they are distinct constraints with different ε and classification. If the difference is rhetorical (both readings oppose the same reforms), they are the same constraint with different narratives, and the sibling distinction is an artifact of committer conceptualization, not constraint structure. For this reading''s classification, it matters whether the anti-tyranny framing genuinely commits to minority protection as a value-neutral procedural matter (federalist) or as a consequence of respecting intrinsic religious authority (communal). The grounding type determines what would falsify or override each reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_underspecification_sibling_reading_pressure, conceptual, 'Whether federalist and communal-autonomy readings are structurally distinct or narratively distinguished versions of the same constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__federalist_millet_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t45, marriage_authority__federalist_millet_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement_basis(marr_tr_t45, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__federalist_millet_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t75, marriage_authority__federalist_millet_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement_basis(marr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority__federalist_millet_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t45, marriage_authority__federalist_millet_reading, base_extractiveness, 45, 0.27).
narrative_ontology:measurement_basis(marr_be_t45, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority__federalist_millet_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t75, marriage_authority__federalist_millet_reading, base_extractiveness, 75, 0.28).
narrative_ontology:measurement_basis(marr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority__federalist_millet_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.13).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t45, marriage_authority__federalist_millet_reading, suppression_requirement, 45, 0.14).
narrative_ontology:measurement_basis(marr_su_t45, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority__federalist_millet_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t75, marriage_authority__federalist_millet_reading, suppression_requirement, 75, 0.15).
narrative_ontology:measurement_basis(marr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (marriage_authority) shared with four sibling readings: communal_autonomy_reading, gender_rights_reading, judicial_harmonization_reading, and secularist_reading. All five constraints describe the same standing institutional arrangement (fragmented family law authority) but instantiate different legitimacy claims and forecast different reform trajectories. The federalist-millet reading justifies fragmentation via anti-majoritarian constitutional federalism; it overlaps with communal_autonomy_reading (both protect minority authority) but differs in grounding (constitutional design vs. religious legitimacy). This reading influences the gender_rights reading (by creating the exclusion that gender-equality advocates must address) and is influenced by the secularist reading (which treats it as a transitional anomaly awaiting elimination). Kernel-reading constraint families require decomposition precisely because unified treatment would collapse ε across incommensurable framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
