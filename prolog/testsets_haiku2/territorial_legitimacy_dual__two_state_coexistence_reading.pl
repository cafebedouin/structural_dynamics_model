% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence: Dual Legitimacy with 1967 Boundaries
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The two-state coexistence reading instantiates one interpretation of
 *   contested legitimacy over the former British Mandate for Palestine:
 *   mutual recognition that both Israeli (1948 statehood) and Palestinian
 *   (self-determination claim) legitimacies are valid, with territorial
 *   partition along 1967 boundaries (pre-1967 War lines) plus land swaps,
 *   security cooperation replacing zero-sum military deterrence, and right of
 *   return limited to Palestinian state territory. This reading is ONE of
 *   three structurally distinct constraint stories from the contested kernel
 *   'territorial_legitimacy_dual.' The sibling readings
 *   (zionist_refuge_reading emphasizing historical persecution as sole
 *   legitimacy source; palestinian_autochthony_reading emphasizing continuous
 *   habitation and displacement as prior claim) are different constraints
 *   with different beneficiary structures and different ε values — not
 *   perspectives on this one. This story describes the two-state reading's
 *   own structural arrangement, extracted costs, and persistence mechanisms.
 *
 * KEY AGENTS:
 *   - Israeli state apparatus: agenda-setter, beneficiary; institutional power; sets security rules, negotiates territorial swaps, enforces framework against settler rejection
 *   - Palestinian state apparatus: agenda-setter, beneficiary; institutional power; co-administers partition, enforces recognition of 1948 fact against autochthony rejection
 *   - International order maintainers: beneficiary; organized, mobile; UN structures, regional powers; collect standing from conflict management and partition success
 *   - Palestinian refugee diaspora: payer, powerless; trapped exit; bears cost of right-of-return limitation to Palestinian state only
 *   - Israeli settlers in disputed territories: payer, organized; identity-locked exit; bear costs of territorial withdrawal and settlement freezes
 *   - Left-behind minorities (Palestinian citizens of Israel, Jewish settlers in Palestinian territories): payer, moderately powered; constrained/identity-locked exit; structurally excluded from both legitimacy axioms
 *   - Zionist-refuge adherents: excluded, organized; identity-locked; would reject mutual_legitimacy_1948 axiom
 *   - Palestinian-autochthony adherents: excluded, organized; identity-locked; would reject mutual_legitimacy_1948 axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.38).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence: Dual Legitimacy with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'f8cef50f-d02c-4dee-a7a3-2beb770a829c').
narrative_ontology:cs_kernel_codification('f8cef50f-d02c-4dee-a7a3-2beb770a829c', formalized).
narrative_ontology:cs_authority_grounding('f8cef50f-d02c-4dee-a7a3-2beb770a829c', extraction).
narrative_ontology:cs_interpretation_layer_present('f8cef50f-d02c-4dee-a7a3-2beb770a829c').
narrative_ontology:cs_reading_relation('f8cef50f-d02c-4dee-a7a3-2beb770a829c', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8cef50f-d02c-4dee-a7a3-2beb770a829c', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('f8cef50f-d02c-4dee-a7a3-2beb770a829c', foundational, mutual_legitimacy_1948).
narrative_ontology:cs_axiom_status(mutual_legitimacy_1948, holdable).
narrative_ontology:cs_axiom_grounding('f8cef50f-d02c-4dee-a7a3-2beb770a829c', mutual_legitimacy_1948, deontological).
narrative_ontology:cs_axiom('f8cef50f-d02c-4dee-a7a3-2beb770a829c', foundational, territorial_partition_1967).
narrative_ontology:cs_axiom_status(territorial_partition_1967, holdable).
narrative_ontology:cs_axiom_grounding('f8cef50f-d02c-4dee-a7a3-2beb770a829c', territorial_partition_1967, conventional).
narrative_ontology:cs_axiom('f8cef50f-d02c-4dee-a7a3-2beb770a829c', secondary, right_of_return_limited_to_palestinian_state).
narrative_ontology:cs_axiom_status(right_of_return_limited_to_palestinian_state, holdable).
narrative_ontology:cs_axiom_grounding('f8cef50f-d02c-4dee-a7a3-2beb770a829c', right_of_return_limited_to_palestinian_state, instrumental).
narrative_ontology:cs_reference_frame('f8cef50f-d02c-4dee-a7a3-2beb770a829c', mutual_legitimacy_partition_1994).
narrative_ontology:cs_drift_state('f8cef50f-d02c-4dee-a7a3-2beb770a829c', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f8cef50f-d02c-4dee-a7a3-2beb770a829c', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_order_maintainers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_in_disputed_territories).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_israel).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_palestine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_compromise_feasibility).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, security_through_mutual_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the two-state framework: sets security parameters, negotiates borders based on 1967+swaps formula, controls military enforcement of territorial boundaries. Collects legitimacy from the framework (recognized by international community, Palestinian Authority acceptance), yet bears security costs of maintaining the compromise against rejection from both hardline settlers and Palestinian groups denying 1948 legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus, beneficiary).

% Co-administers the two-state framework from Palestinian side: negotiates territory claims, administers disputed zones where authority is split, enforces recognition of Israeli statehood in exchange for statehood recognition. Collects legitimacy (UN recognition as non-member state observer, Oslo Accords framework), yet bears costs of enforcing recognition of 1948 fact against Palestinian constituencies viewing Israel as occupier, not legitimate peer.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_apparatus, beneficiary).

% UN structures, regional powers (US, EU), and norm-setting bodies benefit from the two-state framework as a model for partition resolution: it instantiates self-determination, territorial integrity, and negotiated settlement norms. They support enforcement (Quartet mechanisms, Security Council resolutions) and collect diplomatic standing from successful conflict management.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_order_maintainers, beneficiary,
    organized, generational, mobile, global).

% Bears the cost of the framework's right-of-return limitation: the reading restricts return to Palestinian state territory, not to pre-1948 homes inside Israel proper. Diaspora communities in Lebanon, Jordan, Syria, Gulf states experience this as foreclosure of restoration claims. Exit from the constraint means either accepting permanent exile or abandoning the legitimacy framework itself (joining Palestinian-autochthony rejection of the 1948 partition).
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Bear costs of territorial compromise: settlement freezes, territorial withdrawals, security constraints, and eventual dismantling or relocation under the framework's logic. Exits from the constraint mean either: accept relocation (costly), join zionist-refuge reading (rejecting Palestinian legitimacy), or mount armed resistance. Identity-locking operates through religious/ideological fusion with the land (divine promise axiom) and community bonds.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_in_disputed_territories, payer,
    organized, biographical, identity_locked, national).

% Palestinian citizens of Israel experience the framework as partial: they have civil citizenship but the state defines itself through Jewish legitimacy (Law of Return, Jewish majority maintenance). The two-state reading does not address their status within Israel's borders — they are neither returned (diaspora logic) nor fully integrated (state legitimacy logic). Officially included but structurally excluded from the framework's core legitimacy claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_israel, excluded).

% Jewish settlers and historic communities remaining in Palestinian-designated territories experience partial expulsion risk or minority status under Palestinian sovereignty. The framework does not guarantee their minority rights — they are neither fully protected (Israeli security logic) nor fully included (Palestinian legitimacy logic). Structurally excluded from the core compromise between two majoritarian claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_palestine, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, left_behind_minorities_palestine, excluded).

% Constituency rejecting the two-state reading's acceptance of Palestinian legitimacy: view Israel's claim as unilateral (historical persecution justifies exclusive refuge) and see Palestinian acceptance of 1948 borders as illegitimate. Would reject the framework's foundational axiom (mutual_legitimacy_1948) and argue for zionist_refuge_reading instead. Structurally excluded from the consensus this framework requires.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading_adherents, excluded,
    organized, generational, identity_locked, national).

% Constituency rejecting the two-state reading's acceptance of 1948 Israeli legitimacy: view Palestinian claim as prior (continuous habitation, displacement trauma) and see mutual recognition of 1948 borders as capitulation. Would reject the framework's foundational axiom (mutual_legitimacy_1948) and argue for palestinian_autochthony_reading instead. Structurally excluded from the consensus this framework requires.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading_adherents, excluded,
    organized, generational, identity_locked, national).

% Document and advocate for human rights compliance under the framework: monitor refugee conditions, settler displacement, minority protections, security force conduct. They hold the framework accountable to its own humanitarian commitments but do not set the agenda or collect extraction from the arrangement itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, humanitarian_norm_advocates, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a territorial partition problem through mutual recognition: both peoples claim legitimacy on the same land; the framework partitions legitimacy (Israeli within 1967+swaps; Palestinian within 1967 minus swaps, plus right of return to Palestinian state only) so both can exercise self-determination without zero-sum conflict over boundaries.
% TRANSFER_FUNCTION: Transfers territorial claims: Palestinians relinquish claim to lands within 1967 Israeli boundaries and right of return to pre-1948 territory; Israelis relinquish claim to territories beyond 1967 boundaries and implicit claims to Palestinian self-determination. The arrangement moves legitimacy from universality (one people per territory) to partition (two peoples, two territories, shared status).
% ABSENT_VOICES: Zionist-refuge adherents (view 1948 as unilateral legitimacy requiring no Palestinian recognition) and Palestinian-autochthony adherents (view Palestinian claim as prior, 1948 as illegitimate imposition) are excluded from the consensus this framework depends on. Structurally outside the conversation because accepting the framework requires abandoning their core axiom. Diaspora Palestinians rejected by the right-of-return limitation are also absent from formal negotiations, though their interests are claimed to be represented by Palestinian Authority.
% DISAPPEARANCE_RATIONALE: Different parties give incompatible answers. International order maintainers and security-cooperation beneficiaries would see instability: territorial claims would reignite, refugee populations would have no negotiated status, security mechanisms would revert to military deterrence. Zionist-refuge adherents would see liberation (no forced recognition of Palestinian legitimacy). Palestinian-autochthony adherents would see return to direct struggle (no binding partition, no foreclosure of return claims). The constraint's disappearance is contested because the constraint's existence is contested.
% FOUNDING_PROBLEM: After 1948 partition attempt and subsequent military conflicts, both Israeli and Palestinian movements claimed total legitimacy over the same territory; continuing zero-sum competition had produced repeated wars, displacement, and failed negotiations. The two-state framework was built to solve: how can both peoples exercise self-determination without one negating the other?
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state apparatus attests the problem is live: ongoing security threats justify continued enforcement. The Palestinian Authority attests the problem is live: occupation and unresolved status require negotiated settlement. However, zionist-refuge and palestinian-autochthony constituencies attest the founding problem is misconceived — the framework itself is the problem. Academic and humanitarian observers outside both benefiting parties note the founding problem is PARTIALLY solved (wars between state actors are reduced) and PARTIALLY persistent (displacement, refugee status, minority insecurity remain unresolved). No corroboration from outside the benefiting parties for the full-resolution reading; significant testimony that the framework is incomplete.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, contested).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).
:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint solves a genuine coordination problem (territory partition, enabling both peoples to govern) while simultaneously benefiting state apparatus structures at the expense of refugee populations and territorial disputants. Suppression is elevated (0.62) because the framework's persistence depends on actively excluding and criminalizing competing legitimacy readings (rejecting both zionist-refuge and palestinian-autochthony adherents). Theater ratio rises from 0.32 to 0.48 over the interval (Oslo 1994 through 2024) because the framework's functional coordination erodes (security cooperation fails, territorial swaps stall) while theatrical maintenance intensifies (commemorations, agreements, 'peace process' language). The measurement series show extraction rising modestly through 2012 then plateauing, suppression rising to peak in 2012–2018 then declining slightly as state enforcement capacity faces growing resistance, and theater rising consistently as the performative share of institutional activity climbs. The shared time grid on 6-year intervals (aligned across all three metrics) enables temporal analysis of the constraint's lifecycle drift. Coercion grid shows: (1) accessibility_collapse is highest at individual level (settlers, refugees are trapped) and lowest at individual level by 2024 (some mobility options emerge as framework degradation opens alternatives); (2) suppression rises across all levels through 2024 but class-level suppression accelerates (institutional actors feel less pressure than organizational/population-level actors); (3) resistance remains strongest at organizational level (NGOs, political factions) and weakens at individual level (fatigue, normalized displacement).
 *
 * PERSPECTIVAL GAP:
 *   The two state apparatus seats (Israeli and Palestinian) should compute the constraint differently from the payer seats: from the state perspective, this is genuine coordination enabling mutual self-determination; from the refugee/settler perspective, it is enforced extraction — their legitimacy claims are foreclosed by the framework. The international maintainers compute it as successful norm-building (beneficiary frame); excluded constituencies compute it as imposed partition (victim frame). The engine derives directionality from beneficiary/victim declarations; the computed per-seat types should show state apparatus as moderate beneficiaries (d~0.25–0.35), diaspora as targets (d~0.85–0.90), settlers as partly-target/partly-trapped (d~0.65–0.75), excluded adherents as outside-the-constraint (not computed). This divergence IS the point: the framework produces different types from different seats because it is a compromise that coordinates some at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Israeli and Palestinian state apparatus (collect legitimacy, wield enforcement, set terms); international order maintainers (collect diplomatic standing, model for partition resolution). Victims: Palestinian refugee diaspora (foreclosed from return to pre-1948 territory); Israeli settlers in disputed territories (forced territorial withdrawal, identity-locked choices); left-behind minorities (structurally excluded from both legitimacy axioms — Palestinian citizens of Israel are not return-eligible, Jewish settlers in Palestinian areas face expulsion risk or minority status). The refugees sit at the high-target end (d~0.88) because exit means accepting permanent exile or abandoning legitimacy framework. Settlers sit near high-target (d~0.70) because exit is identity-locked (religious/ideological land claims) or requires joining excluded constituencies (zionist-refuge reading). International actors sit at low-target (d~0.10–0.15) — they benefit substantially and face no extraction themselves; exit would mean withdrawing diplomatic support but costs nothing directly. State apparatus sit at beneficiary end (d~0.12–0.20 for Israeli, d~0.18–0.25 for Palestinian) because they collect legitimacy and enforceability, though they bear security costs and suppression costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was: how can both peoples exercise self-determination on the same territory without zero-sum military competition? The two-state framework attempted to solve this by partitioning: Israeli sovereignty over 1967+swaps, Palestinian sovereignty over 1967-swaps plus right of return to Palestinian state. The measurement series and coercion grid show rising theater_ratio (0.32→0.48) concurrent with stable-to-modest extractiveness, which is the mandatrophy signature: the framework's functional coordination (enabling both states to exist, reducing interstate wars) has been substantially achieved, but the framework persists through intensifying theatrical performance (annual agreements, 'peace process' negotiations, commemorations) without resolving the distributional problems (refugee status, settler evacuation, minority protections remain unresolved). The rising suppression_requirement through 2012 then plateau shows the state apparatus must expend growing effort to exclude and criminalize zionist-refuge and palestinian-autochthony readings — the framework is defended less by its coordination success and more by enforcement against competing legitimacy claims. This is classic mandatrophy: a constraint whose original mandate (partition enabling both sovereignties) has been mostly achieved, but whose persistence increasingly depends on theatrical maintenance and active suppression of alternatives rather than on the coordination it was built for. The framework is not dead (both states exist, interstate wars are rare) but is degraded from coordination function toward inertial extraction (state apparatus collecting legitimacy-based authority, international actors collecting norm-success standing, without addressing the distributional costs that motivated the original partition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_mutual_legitimacy_1948_contested,
    'Is the axiom mutual_legitimacy_1948 (recognizing both Israeli and Palestinian statehood claims as valid from 1948 onward) a live normative commitment of this reading, or has it been substantially undermined by on-the-ground practice?',
    'Text-analysis of state apparatus rhetoric (Israeli parliamentary record, Palestinian Authority declarations), treaty language (Oslo Accords, UN recognition votes), and institutional practice (degree to which each state recognizes the other''s legitimacy in law, security cooperation, administrative contact). A substantial gap between declared commitment and institutional practice signals axiom degradation.',
    'If mutual_legitimacy_1948 is substantially undermined, the reading''s distinction from zionist_refuge_reading and palestinian_autochthony_reading collapses — the constraint would reclassify from tangled_rope (asymmetric coordination with extraction) toward snare (pure extraction hiding behind a dead coordination mandate). If axiom remains live, the reading remains structurally distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_mutual_legitimacy_1948_contested, empirical, 'Whether the foundational axiom of mutual recognition is institutionally maintained or theatrically performed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of zionist_refuge and palestinian_autochthony readings structurally imposed (legal barriers, institutional censoring, security force prevention of speech/organizing) or internalized (the readings are culturally marginalized, diaspora-isolated, or intellectually incoherent within the dominant framework)?',
    'Post-framework-collapse scenario: if suppression mechanisms are removed (state apparatus loses enforcement capacity, international order maintainers withdraw support), do excluded constituencies rapidly re-organize around zionist_refuge or palestinian_autochthony readings, or do the readings remain marginalized? Rapid re-organization indicates suppression is structural; continued marginalization indicates internalization.',
    'If suppression is structural, the constraint''s persistence depends on active state enforcement and international support — vulnerability to enforcement collapse. If suppression is internalized, the constraint is more resilient to enforcement decay because the readings lack cultural/intellectual traction independent of suppression. Mapping suppression mechanism affects prediction of the framework''s stability under stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether exclusion of competing readings is maintained by force or by cultural incoherence within the framework.').

omega_variable(
    territorial_partition_1967_naturalness,
    'Is the 1967 boundary line a natural, inevitable, or mathematically optimal partition, or is it a historically contingent artifact of the 1967 War that appears inevitable only through familiarity?',
    'Counterfactual historical analysis: what alternative boundary lines would have enabled mutual self-determination? If multiple boundary configurations could satisfy the founding problem, the 1967 line is contingent (not natural). If the 1967 line is unique or mathematically superior, it is closer to natural.',
    'If the partition line is contingent, then the reading''s second foundational axiom (territorial_partition_1967) is not structural to two-state coexistence — it is one political choice among many. This would weaken the reading''s claim to naturalness and strengthen the argument that it is a particular extraction arrangement benefiting particular state apparatus, not an inevitable coordination solution. If the partition is optimal, the axiom is closer to structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(territorial_partition_1967_naturalness, conceptual, 'Whether the 1967 boundary has special status or is one arbitrary partition among many.').

omega_variable(
    reading_relation_to_zionist_refuge_sibling,
    'What is the precise structural relationship from the two-state coexistence reading to the zionist_refuge reading: does mutual_legitimacy_1948 logically foreclose the zionist_refuge axiom (sole_Israeli_legitimacy), or do they coexist as incompatible but simultaneously-held positions by different actors?',
    'Logical analysis: can a single institutional actor maintain both axioms without contradiction? If the answer is no (accepting mutual legitimacy precludes accepting sole legitimacy), the relation is forecloses. If yes (an actor can shift between them or hold them in different domains), the relation is coexists_with.',
    'If forecloses: the engine will compute strong foreclosure when both readings'' factbases are present, and the winning reading will be determined by cross-factbase coupling. If coexists_with: the readings are allowed to coexist as different institutional positions without logical conflict — the corpus will record both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_zionist_refuge_sibling, conceptual, 'The logical structure of the relationship between this reading and the zionist-refuge-reading sibling.').

omega_variable(
    reading_relation_to_palestinian_autochthony_sibling,
    'What is the precise structural relationship from the two-state coexistence reading to the palestinian_autochthony reading: does mutual_legitimacy_1948 logically foreclose the autochthony axiom (Palestinian_prior_legitimacy), or do they coexist as incompatible but simultaneously-held positions?',
    'Logical analysis: can a single institutional actor maintain both axioms without contradiction? If the answer is no (accepting 1948 legitimacy for both precludes accepting Palestinian legitimacy as prior to 1948), the relation is forecloses. If yes (an actor can frame the legitimacies as sequential or domain-separated), the relation is coexists_with.',
    'If forecloses: the engine will compute strong foreclosure and determine the winning reading by cross-factbase coupling. If coexists_with: the readings coexist as competing institutional positions without logical conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_palestinian_autochthony_sibling, conceptual, 'The logical structure of the relationship between this reading and the palestinian-autochthony-reading sibling.').

omega_variable(
    left_behind_minorities_status_ambiguity,
    'Are Palestinian citizens of Israel and Jewish settlers in Palestinian territories covered by the two-state framework''s legitimacy partition, or are they structurally excluded by a framework designed for state-level legitimacy only?',
    'Text analysis of framework documents, state practice, and institutional capacity: are minority-protection mechanisms built into the framework, or are minority statuses left to ad hoc state administration? If built-in, minorities are covered; if ad hoc, they are excluded.',
    'If minorities are covered, the framework''s victim set is smaller and extraction is more bounded. If minorities are excluded, they represent an unresolved distributional problem that the framework passes to state apparatus rather than solving — a sign of mandatrophy (the partition solves state-level legitimacy but leaves community-level legitimacy unresolved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(left_behind_minorities_status_ambiguity, empirical, 'Whether minority protections are part of the framework''s design or are left to ad hoc state administration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1994, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1994, 0.32).
narrative_ontology:measurement_basis(terr_tr_t1994, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2006, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2006, 0.42).
narrative_ontology:measurement_basis(terr_tr_t2006, observed).
narrative_ontology:measurement(terr_tr_t2012, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2012, 0.46).
narrative_ontology:measurement_basis(terr_tr_t2012, observed).
narrative_ontology:measurement(terr_tr_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2018, 0.49).
narrative_ontology:measurement_basis(terr_tr_t2018, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1994, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1994, 0.28).
narrative_ontology:measurement_basis(terr_be_t1994, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2006, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2006, 0.35).
narrative_ontology:measurement_basis(terr_be_t2006, observed).
narrative_ontology:measurement(terr_be_t2012, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement_basis(terr_be_t2012, observed).
narrative_ontology:measurement(terr_be_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2018, 0.37).
narrative_ontology:measurement_basis(terr_be_t2018, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1994, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1994, 0.48).
narrative_ontology:measurement_basis(terr_su_t1994, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2006, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2006, 0.58).
narrative_ontology:measurement_basis(terr_su_t2006, observed).
narrative_ontology:measurement(terr_su_t2012, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2012, 0.62).
narrative_ontology:measurement_basis(terr_su_t2012, observed).
narrative_ontology:measurement(terr_su_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2018, 0.64).
narrative_ontology:measurement_basis(terr_su_t2018, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(terr_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1994, tn=2024
narrative_ontology:measurement(terr_grid_01, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(class), 1994, 0.72).
narrative_ontology:measurement(terr_grid_02, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(class), 2024, 0.71).
narrative_ontology:measurement(terr_grid_03, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(individual), 1994, 0.78).
narrative_ontology:measurement(terr_grid_04, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(individual), 2024, 0.69).
narrative_ontology:measurement(terr_grid_05, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(organizational), 1994, 0.65).
narrative_ontology:measurement(terr_grid_06, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(organizational), 2024, 0.72).
narrative_ontology:measurement(terr_grid_07, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(structural), 1994, 0.68).
narrative_ontology:measurement(terr_grid_08, territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse(structural), 2024, 0.74).
narrative_ontology:measurement(terr_grid_09, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(class), 1994, 0.64).
narrative_ontology:measurement(terr_grid_10, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(class), 2024, 0.58).
narrative_ontology:measurement(terr_grid_11, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(individual), 1994, 0.48).
narrative_ontology:measurement(terr_grid_12, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(individual), 2024, 0.42).
narrative_ontology:measurement(terr_grid_13, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(organizational), 1994, 0.58).
narrative_ontology:measurement(terr_grid_14, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(organizational), 2024, 0.62).
narrative_ontology:measurement(terr_grid_15, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(structural), 1994, 0.52).
narrative_ontology:measurement(terr_grid_16, territorial_legitimacy_dual__two_state_coexistence_reading, resistance(structural), 2024, 0.48).
narrative_ontology:measurement(terr_grid_17, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(class), 1994, 0.48).
narrative_ontology:measurement(terr_grid_18, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(class), 2024, 0.62).
narrative_ontology:measurement(terr_grid_19, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(individual), 1994, 0.42).
narrative_ontology:measurement(terr_grid_20, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(individual), 2024, 0.48).
narrative_ontology:measurement(terr_grid_21, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(organizational), 1994, 0.52).
narrative_ontology:measurement(terr_grid_22, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(organizational), 2024, 0.68).
narrative_ontology:measurement(terr_grid_23, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(structural), 1994, 0.45).
narrative_ontology:measurement(terr_grid_24, territorial_legitimacy_dual__two_state_coexistence_reading, stakes_inflation(structural), 2024, 0.58).
narrative_ontology:measurement(terr_grid_25, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(class), 1994, 0.54).
narrative_ontology:measurement(terr_grid_26, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(class), 2024, 0.68).
narrative_ontology:measurement(terr_grid_27, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(individual), 1994, 0.62).
narrative_ontology:measurement(terr_grid_28, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(individual), 2024, 0.58).
narrative_ontology:measurement(terr_grid_29, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(organizational), 1994, 0.48).
narrative_ontology:measurement(terr_grid_30, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(organizational), 2024, 0.62).
narrative_ontology:measurement(terr_grid_31, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(structural), 1994, 0.42).
narrative_ontology:measurement(terr_grid_32, territorial_legitimacy_dual__two_state_coexistence_reading, suppression(structural), 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% The constraint territorial_legitimacy_dual instantiates THREE structurally distinct readings of the contested kernel: zionist_refuge_reading (Israeli legitimacy alone), palestinian_autochthony_reading (Palestinian prior legitimacy), and two_state_coexistence_reading (mutual recognition, this constraint). Each has its own ε value, beneficiary structure, type, and foundational axioms. They are linked through the kernel: accepting one reading's axioms forecloses or influences the others. The ε-invariance principle (DP-001) requires separate stories per distinct reading; they are unified by the network.affects_constraints edges, not by a single story with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
