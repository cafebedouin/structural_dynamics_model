% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Territorial Sovereignty Legitimacy — Existential Matrix Reading
 *   domain: political theory/international relations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   territorial_sovereignty_legitimacy: the existential_matrix_reading, which
 *   holds that juridical and historical legitimacy claims are epiphenomenal
 *   to the operative constraint — each people experiences territorial control
 *   as a precondition of collective survival, rendering the conflict zero-sum
 *   and immune to legal settlement. The ε referent is the standing
 *   arrangement under contest, assessed by this reading's own lights: the
 *   layered territorial-control regime (segregated infrastructure, permit
 *   architecture, blockade, barrier, settlement consolidation) plus
 *   reciprocal armed mobilization, read through the reading's claim that
 *   fear, not law, is what binds. Time units are years since 1948. KEY AGENTS
 *   (by structural relationship): israeli_security_establishment:
 *   agenda-setting dominant seat ([institutional]/[identity_locked]) —
 *   administers territorial control, institution fused with the frame;
 *   palestinian_civilian_population: primary target ([powerless]/[trapped]) —
 *   bears displacement, occupation, blockade; israeli_civilian_population:
 *   payer-beneficiary hybrid ([organized]/[constrained]) — buys security with
 *   conscription, taxes, and casualties;
 *   palestinian_armed_resistance_factions: mirror agenda-setter
 *   ([organized]/[identity_locked]) — administers the resistance half of the
 *   matrix, pays in destruction; west_bank_settlement_enterprises and
 *   defense_export_industries: concentrated beneficiaries
 *   ([organized]/[constrained], [institutional]/[arbitrage]);
 *   regional_arab_states: excluded seat ([institutional]/[mobile]);
 *   joint_reconciliation_movements: excluded internal dissent
 *   ([moderate]/[constrained]); international_mediators_and_legal_bodies:
 *   analytical observer ([institutional]/[analytical]). Claim and metrics are
 *   authored independently: the claim is tangled_rope because the reading's
 *   own logic entails a genuine intra-communal survival-coordination function
 *   alongside massive inter-communal asymmetric extraction; the metrics
 *   describe observed operation without reference to that claim.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment: agenda-setting dominant seat ([institutional]/[identity_locked]) — administers territorial facts, institution and survival-mission fused
 *   - palestinian_civilian_population: primary target ([powerless]/[trapped]) — bears the arrangement's costs in land, movement, and life
 *   - israeli_civilian_population: payer with secondary beneficiary position ([organized]/[constrained]) — finances and staffs the arrangement, absorbs attack casualties, receives collective continuity
 *   - palestinian_armed_resistance_factions: mirror agenda-setter with payer costs ([organized]/[identity_locked]) — runs the resistance half of the frame, destroyed and replenished each cycle
 *   - west_bank_settlement_enterprises: concentrated beneficiary ([organized]/[constrained]) — asset base exists only while territorial control holds
 *   - defense_export_industries: parasitic beneficiary ([institutional]/[arbitrage]) — monetizes continuation, hedged against any resolution
 *   - regional_arab_states: excluded ([institutional]/[mobile]) — exited the decision loop via normalization, hosts refugees it will not absorb
 *   - joint_reconciliation_movements: excluded ([moderate]/[constrained]) — internal dissent testifying the frame is contingent, no seat anywhere
 *   - international_mediators_and_legal_bodies: analytical observer ([institutional]/[analytical]) — registers the juridical impotence this reading predicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.8).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty Legitimacy — Existential Matrix Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political theory/international relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '015aeafe-2f59-47b7-9857-e6eb8764d249').
narrative_ontology:cs_kernel_codification('015aeafe-2f59-47b7-9857-e6eb8764d249', distributed).
narrative_ontology:cs_authority_grounding('015aeafe-2f59-47b7-9857-e6eb8764d249', practice).
narrative_ontology:cs_reading_relation('015aeafe-2f59-47b7-9857-e6eb8764d249', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('015aeafe-2f59-47b7-9857-e6eb8764d249', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_axiom('015aeafe-2f59-47b7-9857-e6eb8764d249', foundational, collective_survival_requires_territorial_control).
narrative_ontology:cs_axiom_status(collective_survival_requires_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('015aeafe-2f59-47b7-9857-e6eb8764d249', collective_survival_requires_territorial_control, empirically_contingent).
narrative_ontology:cs_axiom('015aeafe-2f59-47b7-9857-e6eb8764d249', foundational, juridical_legitimacy_epiphenomenal_under_existential_threat).
narrative_ontology:cs_axiom_status(juridical_legitimacy_epiphenomenal_under_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('015aeafe-2f59-47b7-9857-e6eb8764d249', juridical_legitimacy_epiphenomenal_under_existential_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('015aeafe-2f59-47b7-9857-e6eb8764d249', mutual_existential_deterrence_equilibrium).
narrative_ontology:cs_drift_state('015aeafe-2f59-47b7-9857-e6eb8764d249', post_october_2023_escalation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('015aeafe-2f59-47b7-9857-e6eb8764d249', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, west_bank_settlement_enterprises).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, defense_export_industries).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_civilian_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_civilian_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_civilian_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_armed_resistance_factions).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, security_dilemma_theory).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, zero_sum_territorial_competition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the army, intelligence services, and civil administration that govern territory, borders, and permits. Organizes budget, doctrine, promotion ladders, and reserve obligations entirely around preventing any recurrence of past attempts at the community's destruction. Sets the physical facts on the ground — settlement expansion, barrier routing, rules of engagement — and prices every territorial concession as measurable mortal risk to members' own families. The institution's purpose and the population's survival expectation have fused; abandoning the frame would leave it an ordinary defense ministry with no extraordinary mandate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).

% Lives under layered restriction: segregated road networks, permit regimes, blockade, periodic large-scale military operations, and house demolitions. Cannot move, build, farm, or access water on equal terms; a large registered fraction lives as refugees abroad without return rights. Leaving permanently forfeits residency and family land; neighboring states decline absorption; staying means absorbing the casualties, curfews, and economic strangulation of each escalation. No available action removes the condition.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_civilian_population, payer,
    powerless, generational, trapped, regional).

% Conscripts its children for multi-year service plus decades of reserve duty, carries a defense-dominant tax burden, and absorbs rocket fire, infiltrations, and attack casualties in each escalation round. In exchange it holds citizenship in a state whose territory and military supremacy secure its collective continuity and refuge status worldwide. Emigration is genuinely available and common enough to have a vernacular name, but carries heavy social stigma and family rupture; most who can afford exit stay anyway.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_civilian_population, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_civilian_population, beneficiary).

% Operate armed wings and parallel governance — tunnels, arsenals, border breaches, welfare patronage — from Gaza and fragments of the West Bank political field. Their internal legitimacy rests on refusing to concede the other community's permanence and on armed capability as proof of refusal. Each fighting round destroys their infrastructure, kills their fighters and constituents, and simultaneously replenishes the grievance capital and recruitment pool they depend on. Disarming would convert them into ordinary political parties competing on services, a conversion their founding charters and rivalries resist.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_armed_resistance_factions, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_armed_resistance_factions, payer).

% Build and expand housing, agriculture, light industry, and municipal institutions beyond the recognized line, financed by state subsidies, permits, and bypass-road access. Their asset base, schools, and way of life exist only while territorial control holds; prior evacuation episodes meant forcible removal of communities that had priced permanence into generations of investment. They lobby continuously for expansion and against any line-drawing that strands them.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, west_bank_settlement_enterprises, beneficiary,
    organized, generational, constrained, regional).

% Sell weapons, surveillance systems, munitions, and doctrine to the region's patrons and client states. Perpetual insecurity produces perpetual demand, and recurring combat provides battlefield-tested marketing claims no competitor can replicate. Exposure to any particular diplomatic breakthrough is hedged across global markets; they need neither side to win, only the conflict to continue.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, defense_export_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Formally champion the stateless people's cause while having signed peace treaties and normalization agreements with the dominant state, and while declining to absorb the refugee populations they host. They retain rhetorical investment and occasional leverage but have largely removed themselves from the conflict's decision loop; their populations remain emotionally engaged, limiting how far governments can trade away the cause openly.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_arab_states, excluded,
    institutional, generational, mobile, continental).

% Grassroots groups of bereaved families and veterans from both communities who meet, mourn together, and argue publicly that the shared fear-frame — not the other people — is the operative enemy. Shouted down in wartime on both sides, dependent on foreign philanthropy, holding no formal seat in any negotiation track, and treated by each community's security mainstream as naive at best and subversive at worst.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, joint_reconciliation_movements, excluded,
    moderate, biographical, constrained, regional).

% UN organs, the Quartet, the ICJ and ICC, and third-party mediator states issue resolutions, convene conferences, publish reports, and impose symbolic sanctions. Decades of outputs have not altered facts on the ground; their analytic distance lets them register what participants deny — that legal instruments have tracked power rather than shaped it — while their institutional incentives keep them producing paper anyway.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_mediators_and_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_security_establishment).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within each national community, the existential-security frame solves a real mobilization problem: it unifies fractious, ideologically split populations around defense priorities, justifies conscription, taxation, and endurance of hardship, and maintains boundary discipline against internal defection. Across the two communities it coordinates nothing except reciprocal deterrence — each side's preparedness is the input to the other's.
% TRANSFER_FUNCTION: Moves land, water, building rights, movement freedom, and security margin from the subordinated population to the territorially dominant party's control apparatus; and moves blood, treasure, and civic freedom from both civilian publics into each side's respective security sector.
% ABSENT_VOICES: Joint reconciliation movements, binational and confederal advocates, and the refugee populations themselves are marginalized in both publics and absent from every negotiation track. Regional states have physically exited via normalization while retaining rhetorical stakes. The dead of each escalation round — the strongest possible witnesses against the frame's continuation — have no seat anywhere. They are outside the security-dominated public spheres of both communities, kept out by wartime censorship dynamics, communal shaming, and the absence of any forum their testimony could reach.
% DISAPPEARANCE_RATIONALE: Under this reading's own premises, the matrix is the operative driver, so its overnight dissolution would rearrange everything: with territorial control no longer coded as survival precondition, security guarantees substitute for territory, negotiated sharing stabilizes within years, settlement growth loses its protective rationale, and legal instruments suddenly acquire purchase because fear stops vetoing them. Hardliners on both sides dispute this, asserting that religious-demographic commitments would persist regardless — the verdict is authored from this reading's seat while recording that the parties themselves disagree about whether the world would rearrange.
% FOUNDING_PROBLEM: Two peoples each faced plausible collective-death scenarios in the same small territory: the destruction of European Jewry immediately preceding statehood, the war and mass displacement of 1947-49, declared rejectionist intent by surrounding states in the 1950s-60s, and the stateless population's experience of expulsion and dispossession in the same events. The matrix was forged by real threats on both sides, not invented cover.
% FOUNDING_PROBLEM_CORROBORATION: External to the benefiting parties: the historical archive corroborates the original threats were real — Holocaust documentation, UN partition deliberations, 1948 Arab League communiques, and contemporary official statements from the dominant state's declared adversaries. Independent historians working from opposed vantage points (Israeli new historians, Palestinian institutional histories) corroborate both the reality of the early threats and the disputed question of whether present-day fears track present-day threats or reproduce them institutionally. Both sides' current leaderships attest the problem's liveness, but their attestation is self-serving; the outside-the-beneficiary corroboration is archival and historiographic, not political.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (ε = 0.80 at interval end) because the arrangement transfers land, water, movement, and autonomy continuously from the subordinated population while drawing blood, treasure, and civic freedom from both publics, with no settlement horizon in which those flows would reverse. Suppression is higher still (0.88) because persistence depends on administered coercion — checkpoints, permits, blockade, barrier, periodic operations — plus wartime information control in both publics; roughly seventy percent of the suppressive force is structural (military-administrative) and thirty percent internalized (fear conditioning transmitted through education, memorial culture, and media on both sides). Theater ratio ends at 0.30: the Oslo-era negotiation layer was substantially performative (peak 0.55 at t=44 while settlement construction doubled), whereas the coercive layer is brutally functional; after 2023 the theatrical pretense largely collapsed into open war-making. The three metric series run on ONE shared grid (years 0, 11, 22, 33, 44, 55, 66, 77). The trajectories oscillate rather than drift monotonically — war cycles drive extraction and suppression spikes (1948, 1967-70, First Intifada, Second Intifada, Gaza rounds, post-October-2023) with partial relaxation between. Critically, the oscillation is not noise: each round functions as intermittent reinforcement for the frame itself, since every war 'proves' the existential premise to both publics and widens the maximalist coalition. Base-property scalars are end-state (t=77) values of these series.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structure. From the dominant security seat, the arrangement is survival coordination it personally guarantees: concessions read as mortal risk, so the seat experiences the frame as a rope it would be criminally negligent to drop. From the subordinated civilian seat, the same structure is enforced dispossession with no exit: the seat experiences a snare whose escape routes (flight, surrender, assimilation) all destroy the community. The dual-positioned civilian seats compute ambivalence — paying in children and taxes while receiving continuity. The observer seat sees what this reading itself predicts: juridical instruments tracking power rather than shaping it. The engine computes this per-seat divergence from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low-derived directionality: the security establishment sits nearest the beneficiary pole as the seat administering and collecting territorial control; settlement enterprises and arms industries collect rents without running the core arrangement. Declared victims map to high directionality: the trapped subordinated civilian population sits effectively at the full-target end — trapped exit amplifies χ — while the Israeli civilian public sits mid-range, genuinely paying (conscription, casualties, taxes) while incidentally receiving continuity, which is why it is declared victim-with-secondary-benefit rather than beneficiary. Two derivations deserve scrutiny. First, the security establishment is declared a beneficiary but is also identity-fused and authentically threat-exposed — its true structural position is part-target (institutional survival and members' families ride on the frame), so its effective extraction contribution is lower than a pure rentier's; the per-power-atom override surface cannot isolate one institutional agent without distorting the other institutional seats (exporters, regional states, observers), so no override is authored and this correction is recorded here and in the omega on fear authenticity. Second, the resistance factions administer their half of the matrix (agenda-setter position) while being destroyed by its operation (payer costs) — a genuinely bidirectional seat the scalar derivation can only approximate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real, mutually credible annihilation threats converging on one territory — was authentic, which is precisely what makes this constraint resistant to both mislabels. Calling it a pure snare erases the genuine intra-communal survival coordination the frame delivers to each public (mobilization, sacrifice justification, boundary discipline under credible threat); calling it a pure rope erases the enormous asymmetric extraction the same structure imposes on the subordinated population. A piton reading fails outright: the function is not atrophied but brutally live. Mandatrophy is NOT declared resolved — the reading itself holds the founding problem live, and the mismatch consumer finds status=live paired with verdict=world_rearranges, an internally consistent pairing with no zombie flag. The residual mandatrophy question — whether the frame now reproduces the fear it was built to answer faster than threats regenerate it — is carried by the fear-authenticity omega rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the territorial_sovereignty_legitimacy kernel is operative for any given actor — the existential matrix (this file), covenant continuity, or demographic self-determination?',
    'Comparative predictive test across the interval: do legal instruments (Oslo accords, UN resolutions, ICJ determinations) ever precede and cause changes in facts on the ground, or do they only ratify power shifts after the fact? Behavioral response to legal settlement offers versus behavioral response to military-demographic shifts separates the readings.',
    'If a juridical reading is operative for the relevant actors, the constraint reclassifies toward a legal-regime rope with a completely different beneficiary set (recognition-seeking polities, international-law professions) and a far lower epsilon referent; this file''s classification holds only to the extent the existential driver is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This story is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and beneficiary structures.').

omega_variable(
    naturalness_of_territorial_requirement,
    'Is the claim that each people requires territorial control for collective survival a structural regularity of collective human existence, or a historically constructed, mutually reinforcing frame that presents itself as natural law?',
    'Survey counterfactual cases: diaspora nations sustaining identity across centuries without sovereignty (pre-state Jews themselves), indigenous nations under alien sovereignty, multinational federations, autonomous arrangements short of exclusive territorial control. If collective survival and identity expression commonly persist without territorial control, the requirement is constructed rather than natural.',
    'If constructed, the frame''s apparent naturality is cover and effective extraction rises accordingly, with identifiable beneficiaries capturing the surplus (false-summit dynamics); if it reflects a real regularity, the constraint approaches mountain character — persistent regardless of enforcement, with compromise frameworks unstable as a matter of structure rather than politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_territorial_requirement, empirical, 'Whether the zero-sum territorial requirement is natural law or constructed frame — the naturalness ambiguity beneath this reading''s core axiom.').

omega_variable(
    fear_authenticity_drift,
    'Does the dominant seat''s existential fear presently track objective threats, or has it become institutionally self-perpetuating — reproduced by the very apparatus that exists to answer it?',
    'Independent threat assessment outside the security establishment: adversary capability and intent data evaluated by analysts with no career dependence on the frame, compared against the establishment''s published threat inflation over successive budget cycles.',
    'If fear tracks real threats, the frame retains genuine survival-coordination content and the tangled_rope reading stands; if it has become self-perpetuating, the coordination function is atrophying behind institutional reproduction and the classification drifts toward snare — extraction serving institutional persistence rather than survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fear_authenticity_drift, empirical, 'Whether the operative driver is still authentic threat perception or institutional self-reproduction of threat perception.').

omega_variable(
    symmetry_of_zero_sum_binding,
    'Does the zero-sum structure bind both peoples symmetrically as the reading''s universal phrasing implies, or asymmetrically — one side''s survival secured by the arrangement while the other''s remains denied?',
    'Compare objective existential indicators across the two communities: recognized statehood, military capacity, demographic trajectory, refugee return prospects, per-capita casualty exposure in recent rounds, and each side''s ability to survive the other''s worst credible scenario.',
    'If binding is asymmetric, the ''both peoples trapped in one matrix'' framing conceals a dominator-dominated structure and the classification slides toward snare with the subordinated population as sole victim; if genuinely symmetric, the dual-victim declaration stands and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_of_zero_sum_binding, empirical, 'Whether the matrix traps both peoples equally or secures one at the other''s expense — tests the reading''s own symmetry claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t11, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 11, 0.2).
narrative_ontology:measurement_basis(terr_tr_t11, observed).
narrative_ontology:measurement(terr_tr_t22, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 22, 0.28).
narrative_ontology:measurement_basis(terr_tr_t22, observed).
narrative_ontology:measurement(terr_tr_t33, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 33, 0.42).
narrative_ontology:measurement_basis(terr_tr_t33, observed).
narrative_ontology:measurement(terr_tr_t44, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 44, 0.55).
narrative_ontology:measurement_basis(terr_tr_t44, observed).
narrative_ontology:measurement(terr_tr_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 55, 0.48).
narrative_ontology:measurement_basis(terr_tr_t55, observed).
narrative_ontology:measurement(terr_tr_t66, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 66, 0.4).
narrative_ontology:measurement_basis(terr_tr_t66, observed).
narrative_ontology:measurement(terr_tr_t77, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 77, 0.3).
narrative_ontology:measurement_basis(terr_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t11, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 11, 0.52).
narrative_ontology:measurement_basis(terr_be_t11, observed).
narrative_ontology:measurement(terr_be_t22, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 22, 0.66).
narrative_ontology:measurement_basis(terr_be_t22, observed).
narrative_ontology:measurement(terr_be_t33, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 33, 0.58).
narrative_ontology:measurement_basis(terr_be_t33, observed).
narrative_ontology:measurement(terr_be_t44, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 44, 0.54).
narrative_ontology:measurement_basis(terr_be_t44, observed).
narrative_ontology:measurement(terr_be_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 55, 0.74).
narrative_ontology:measurement_basis(terr_be_t55, observed).
narrative_ontology:measurement(terr_be_t66, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 66, 0.68).
narrative_ontology:measurement_basis(terr_be_t66, observed).
narrative_ontology:measurement(terr_be_t77, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 77, 0.8).
narrative_ontology:measurement_basis(terr_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t11, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 11, 0.62).
narrative_ontology:measurement_basis(terr_su_t11, observed).
narrative_ontology:measurement(terr_su_t22, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 22, 0.7).
narrative_ontology:measurement_basis(terr_su_t22, observed).
narrative_ontology:measurement(terr_su_t33, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 33, 0.74).
narrative_ontology:measurement_basis(terr_su_t33, observed).
narrative_ontology:measurement(terr_su_t44, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 44, 0.66).
narrative_ontology:measurement_basis(terr_su_t44, observed).
narrative_ontology:measurement(terr_su_t55, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 55, 0.82).
narrative_ontology:measurement_basis(terr_su_t55, observed).
narrative_ontology:measurement(terr_su_t66, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 66, 0.84).
narrative_ontology:measurement_basis(terr_su_t66, observed).
narrative_ontology:measurement(terr_su_t77, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 77, 0.88).
narrative_ontology:measurement_basis(terr_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who legitimately owns this territory' decomposes into three structurally distinct constraints with different epsilon, beneficiary structures, and failure modes. covenant_continuity_reading (theological-lineage grounding, faith-dependent, insensitive to power facts), self_determination_reading (demographic-juridical grounding, implementation-dependent, eroded by facts on the ground), and this existential_matrix_reading (material-existential grounding, empirically contingent axioms, treats the other two as epiphenomenal). The upstream juridical readings supply the legitimacy discourse whose causal idleness this reading asserts; the facts this reading foregrounds flow downstream and destroy the viability conditions of the partition-based sibling. Each member of the family links to the others via affects_constraints; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
