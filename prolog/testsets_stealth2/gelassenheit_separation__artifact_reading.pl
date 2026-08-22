% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Visible-Marker Separation Regime (Artifact Reading of Gelassenheit)
 *   domain: religious/technological/commitment-system
 *
 * SUMMARY:
 *   In the strictest Amish affiliations, separation from the 'English' world
 *   is administered as a visible-marker regime: the Ordnung bars technologies
 *   and materials by their resemblance to worldly artifacts regardless of
 *   function — solar panels are refused on farms that are fully off the grid,
 *   modern fabrics are refused though plainer cloth costs more and wears
 *   worse, and exposed engines fail where the same engines enclosed in black
 *   boxes pass. This file instantiates ONE reading of the
 *   gelassenheit_separation kernel, the artifact_reading, as a clean
 *   epsilon-invariant constraint: the test is appearance, and the referent of
 *   the authored epsilon is the standing marker regime as this reading holds
 *   it, assessed from the analytical seat. The claim and the metrics are
 *   independent authored facts: the constraint is CLAIMED as tangled_rope
 *   (genuine boundary coordination plus asymmetric extraction under active
 *   enforcement) while the metrics describe high extraction and near-maximal
 *   suppression — the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computed type is
 *   the measurement the corpus exists to take. The temporal grid maps
 *   interval point 0 to approximately 1965 and point 60 to approximately
 *   2025, tracking the strictest-affiliation stratum, where hardening has
 *   been monotonic; the broader Amish system cycles through schism, with
 *   lenient splits absorbing relaxation while the strict core re-hardens.
 *
 * KEY AGENTS:
 *   - - ordained_district_ministry: Agenda setter (institutional/identity_locked) — administers the marker regime, rules on resemblance, collects deference and interpretive finality
 *   - - amish_farming_households: Primary target (moderate/constrained) — absorbs appearance-based refusals as labor, cost, and forgone efficiency
 *   - - rumspringa_youth: Target at the commitment threshold (powerless/mobile) — faces the marker list as the price of membership, with no council seat
 *   - - baptized_community_members: Dual beneficiary/payer (organized/constrained) — receives boundary legibility and mutual aid, pays household by household
 *   - - shunned_nonconformists: Excluded voice (powerless/trapped) — sees the regime's costs most clearly and may not speak
 *   - - scholars_of_anabaptism: Analytical observer (analytical/analytical) — traces the reading's genealogy and tracks affiliation drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.72).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.86).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Visible-Marker Separation Regime (Artifact Reading of Gelassenheit)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technological/commitment-system").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '6d68d58f-d944-43e3-bc3b-5303ac6bf4f6').
narrative_ontology:cs_kernel_codification('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', fixed_text).
narrative_ontology:cs_authority_grounding('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', lineage).
narrative_ontology:cs_interpretation_layer_present('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6').
narrative_ontology:cs_reading_relation('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', gelassenheit_separation__principle_reading, forecloses).
narrative_ontology:cs_reading_relation('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', gelassenheit_separation__consequence_reading, forecloses).
narrative_ontology:cs_axiom('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', foundational, visible_distinction_constitutes_separation).
narrative_ontology:cs_axiom_status(visible_distinction_constitutes_separation, holdable).
narrative_ontology:cs_axiom_grounding('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', visible_distinction_constitutes_separation, deontological).
narrative_ontology:cs_axiom('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', secondary, worldly_appearance_outweighs_function).
narrative_ontology:cs_axiom_status(worldly_appearance_outweighs_function, holdable).
narrative_ontology:cs_axiom_grounding('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', worldly_appearance_outweighs_function, deontological).
narrative_ontology:cs_reference_frame('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', schleitheim_visible_separation).
narrative_ontology:cs_drift_state('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', contemporary_strictest_stratum, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6d68d58f-d944-43e3-bc3b-5303ac6bf4f6', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, ordained_district_ministry).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, baptized_community_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_farming_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, rumspringa_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, baptized_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and ministers of each district receive the Ordnung by transmission from preceding generations and administer it: they convene the twice-yearly council where technology questions are raised, rule on whether a proposed item resembles English usage, and oversee the discipline of members who proceed without approval. Their standing, livelihood, and family bonds lie wholly inside the community; stepping away from the marker regime would dissolve the office they hold. Deference and interpretive finality flow to them from the arrangement they administer.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordained_district_ministry, agenda_setter,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, ordained_district_ministry, beneficiary).

% Household heads farm with horse-drawn equipment under rules that bar machinery and materials by their look rather than their wiring: a diesel engine enclosed in a black box may pass where the same engine exposed fails, and rooftop solar arrays are refused even on farms fully off the grid because the panels read as English infrastructure. Plain dark fabrics are required where modern synthetics would be cheaper and longer-wearing. Each refusal lands as extra labor, forgone efficiency, or higher cost. A household that finds the burden intolerable can appeal at council, migrate to a more lenient affiliation, or leave — each path costing standing, kinship ties, or the family's place in the community economy.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_farming_households, payer,
    moderate, biographical, constrained, local).

% Teenagers in the running-around years sample English life — cars, phones, city work — before deciding on baptism. The choice is framed entirely in visible terms: joining means taking up plain dress and surrendering the looked-for items permanently, and the community reads a youth's seriousness by how quickly the outward markers are adopted. During the window they may walk away with little penalty; after baptism the same departure becomes a shunned one. They hold no vote in council and no seat where the marker list is argued.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, rumspringa_youth, payer,
    powerless, immediate, mobile, local).

% Members in good standing receive what the visible order secures: a legible boundary that keeps the community a recognizable people across scattered settlements, dense mutual-aid obligations activated by barn raisings and disaster recovery, marriage within the faith, and an assured place in the salvation framework the markers express. The same members pay for it household by household — in labor, in items surrendered, in the quiet watching of neighbors' barns and closets. Recourse runs through council petition and affiliation choice, both costly.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, baptized_community_members, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, baptized_community_members, payer).

% Members under the ban — for adopting a forbidden item, disputing a ruling, or leaving after baptism — are cut off: meals may not be eaten at their table, commercial dealings narrow, and family members limit contact. They hold the clearest view of what the marker regime costs and the least standing to say it; re-entry runs through public submission to the very ruling they objected to.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, shunned_nonconformists, excluded,
    powerless, biographical, trapped, local).

% Historians and sociologists of Anabaptist religion trace the visible-separation discipline to sixteenth-century confessional documents and track how affiliation splits redistribute strictness across the Amish world. They publish on the distance between Gelassenheit as inner yieldedness and its administration as outward marking; their work circulates back into the communities mainly through leavers and sympathetic clergy.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, scholars_of_anabaptism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, ordained_district_ministry).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the boundary-maintenance problem of a dispersed agrarian minority embedded in a numerically dominant host society: shared visible markers make membership mutually legible across districts, coordinate dress, building, and equipment so any member can recognize any other at a glance, and give the ministry a single administrable standard in place of case-by-case judgment about each new artifact.
% TRANSFER_FUNCTION: Moves decision rights over household technology and dress from households to district councils; moves labor and forgone efficiency from member households to the boundary project; moves deference and interpretive finality to the ordained ministry; moves conformity displays from individuals into the community's collective identity.
% ABSENT_VOICES: Shunned nonconformists and post-baptism leavers hold the sharpest testimony about what the marker regime costs and are structurally voiceless — re-entry requires submitting to the rulings they disputed. Pre-baptism youth have no council seat. Women speak at council through their household heads. Outside scholars who read Gelassenheit as inner yieldedness are heard mainly secondhand, filtered through leavers.
% DISAPPEARANCE_RATIONALE: Without the visible-marker regime the community's legible boundary dissolves within a generation: mutual-aid coordination weakens as members lose the ability to read one another's commitments at a glance, intermarriage and market integration accelerate, the ministry loses the administrable standard on which its office rests, and the Amish as a distinct people converge toward their assimilated Mennonite and Beachy cousins. The rearrangement is precisely what the regime exists to prevent.
% FOUNDING_PROBLEM: Keep a counter-cultural church a distinct people 'in the world but not of it': sixteenth-century Anabaptists faced persecution for refusing conformity, and the Schleitheim Confession (1527) commanded a separation that could be seen — visible nonconformity as the mark of the true church.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of Anabaptism working from the Schleitheim and Dordrecht confessional texts, and sociologists of the Amish (Hostetler, Kraybill and successors), independently attest that the separation problem the regime answers was real at its founding and remains theologically live; the community's own documents state it, but the academic attestation does not depend on the ministry's account.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the appearance test is decoupled from function: it denies households technologies that would impose no entanglement cost at all under the reading's own terms, converting pure boundary signaling into real labor and opportunity cost borne by specific households. Suppression is near-maximal (0.86) because persistence rests on Meidung — total social cutoff of the noncompliant — layered on economic dependence on community land and markets and on salvation stakes; roughly two-thirds of this is structural (ban mechanics, economic closure) and one-third internalized (formed aversion), with the split carried as an omega rather than resolved into the scalar. Theater is moderate-low (0.30): the markers are functionally load-bearing for boundary legibility and mutual aid, but a growing share of activity polices appearance detached from inner state, and the trend is upward. Accessibility collapse is moderate (0.50): alternatives do not vanish — New Order, Beachy, and Mennonite affiliations exist, and the Ordnung itself is revisable at council — but every alternative is costly in kinship and standing. Resistance is moderate (0.42): youth pushback during the running-around years, periodic Ordnung disputes, affiliation migration, and occasional litigation. The measurement series run on one shared time grid (every tracked metric authored at every point 0-60) so the engine samples a complete matrix; the monotonic rise models the strict stratum specifically, since schism exports the system-wide oscillation to lenient affiliates while the strict core accumulates.
 *
 * PERSPECTIVAL GAP:
 *   SEAT DIVERGENCE: from the ordained ministry's seat the regime is faithful stewardship of an inherited trust — the same structure that computes as extractive from the household seat computes as legitimate administration from theirs, because the ministry collects deference and never bears the refused harvest efficiencies. Households experience concrete denials with names and prices; youth experience the marker list as the admission price to everything they love; shunned members experience it as total social death. The engine computes these divergent per-seat classifications from power, exit, and directional position — the authored claim does not adjudicate among them. IDENTITY LOCK: the ministry's lock is institutional (the office has become its function — a bishop who dismantled the marker regime would unmake his own authority); household lock is relational-economic (kinship, land, livelihood); youth lock is prospective (baptism converts a mobile window into a trapped position). If the identity frame broke — if Gelassenheit were widely retaught as inner yieldedness indifferent to form — the ministry's authority would dissolve first and the regime would follow.
 *
 * DIRECTIONALITY LOGIC:
 *   The ministry sits nearest the beneficiary end: it collects deference and interpretive finality and bears almost none of the regime's material costs, and its identity lock sustains the subsidy indefinitely. Baptized community members sit low-to-symmetric: genuine coordination receipts (legibility, mutual aid, marriage pool, salvation assurance) against diffuse household costs — hence the dual beneficiary/payer authorship. Farming households sit near the target end: they bear the transfer in labor and forgone efficiency with only constrained exit. Rumspringa youth are targets at the commitment threshold; their formally mobile window would lead a mobility-keyed derivation to understate their exposure, so a directionality override lifts the powerless atom to 0.78 — the correct value for both powerless seats, since the shunned nonconformists (trapped, maximal exposure) and the youth (pressured toward irreversible assumption of the marker burden) are both targets despite opposite exit profiles. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by directionality and the local spatial scope of enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a persecuted counter-church a distinct people — is still live inside the tradition, so mandatrophy is not resolved and no sunset applies. The tangled_rope classification prevents two symmetrical mislabels: a pure-coordination reading would erase the asymmetric extraction (the ministry's concentrated authority receipts against households' diffuse material costs, enforced by shunning), while a pure-extraction reading would erase the genuine coordination the same structure delivers (mutual aid, boundary legibility, and community continuity demonstrably depend on the visible markers). The lifecycle risk sits elsewhere: if the founding problem ever died — through completed assimilation or doctrinal abandonment — the marker regime would likely persist by inertia and performance, at which point the theater ratio becomes the leading indicator and the piton cell (prohibitive fixing cost with diffuse residual function) the relevant terminal state. The current profile is not that: enforcement is active, function is real, and the extraction has a named collector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the gelassenheit_separation kernel — artifact, principle, or consequence — correctly instantiates the separation mandate the community inherits?',
    'District council deliberation records, affiliation migration patterns between strict and lenient Amish and Mennonite bodies, and confessional-source scholarship on what Schleitheim-era nonconformity was taken to require.',
    'If the principle reading prevails, functionally isolated technology is admitted and this reading''s victim set largely evaporates; if the consequence reading prevails, evaluation turns on effects on visiting, mutual aid, and rootedness, and the appearance list loses its decisive force. Either shift drops measured extraction sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This story is one reading of a contested kernel; the sibling readings instantiate structurally different constraints with different victim sets and epsilon.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (shunning, economic dependence on community land and markets, kinship loss) or internalized (Gelassenheit formation making self-limitation feel like devotion)?',
    'Post-exit trajectory studies comparing leavers with voluntary adult converts: if self-limitation persists essentially unchanged after exit removes the enforcement machinery, the internalized share is large.',
    'If largely internalized, effective suppression exceeds the structural measure — members carry the regime with them after exit and the exit option is worth less than it appears; if structural, removing shunning would relax compliance quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a consent-based community regime.').

omega_variable(
    marker_visibility_theater_status,
    'Is appearance-maintenance theater relative to Gelassenheit''s inner-yieldedness ideal, or is visible form constitutive of the devotion itself?',
    'Compare preaching emphases and disciplinary cases across districts: if discipline targets motives and heart-posture alongside objects, the markers are expressive; if only objects and appearances are ever disciplined, maintenance is performative.',
    'If constitutive, the theater ratio is structurally misdescriptive and the regime is more functional than scored; if theatrical, the regime is drifting toward performing separation without its substance, and piton dynamics become the relevant lifecycle risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marker_visibility_theater_status, conceptual, 'Whether the visible-marker regime is expressive devotion or performative maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__artifact_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__artifact_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__artifact_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__artifact_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__artifact_reading, suppression_requirement, 50, 0.83).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__artifact_reading, suppression_requirement, 60, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Amish technology rules' decomposes into three structurally distinct constraints sharing one kernel (gelassenheit_separation): this artifact reading (appearance test; forbids functionally harmless off-grid technology; high epsilon, maximal suppression), the principle_reading (entanglement test; admits isolated technology; materially lower epsilon), and the consequence_reading (relational-effects test; victim set defined by harms to visiting, mutual aid, and rootedness). Epsilon differs sharply across the family because the victim sets differ, so the label was disambiguated into three files linked by network.affects_constraints rather than forced into one observable-dependent story. The upstream confessional lineage (Schleitheim 1527) is cited as evidence by all three readings; this strictest reading exerts downstream pressure on the others as the boundary case that defines how far lenient affiliates may drift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
