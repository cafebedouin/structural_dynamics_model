% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Claim on Hagia Sophia (Byzantine-Origin Reading)
 *   domain: cultural heritage/religious authority/sovereignty
 *
 * SUMMARY:
 *   This story authors ONE reading of the hagia_sophia_substrate kernel: the
 *   claim that the site's legitimacy derives from its 537 consecration as the
 *   Great Church of the Ecumenical Patriarchate, and that it should therefore
 *   return to Orthodox ecclesiastical control or, failing that, be held in
 *   reverent neutrality. The standing arrangement under contest — the thing
 *   this story measures — is the claim itself as continuously maintained by
 *   the Patriarchate, the Greek state, and the diaspora: an external
 *   normative position with no coercive apparatus, no realistic
 *   implementation pathway, and yet real operative effects (standing
 *   sovereignty contestation, recurring diplomatic friction, identity
 *   infrastructure on both sides of the Aegean). Epsilon's referent is this
 *   maintained claim, assessed by the reading's own lights: its holders
 *   regard it as righteous advocacy, so the authored epsilon is low in
 *   material terms and reflects the symbolic imposition the claim projects
 *   onto its targets. The sibling readings (islamic_sovereignty_reading,
 *   universal_heritage_reading) are separate constraints with their own
 *   epsilon and beneficiary/victim structures; they are linked, not averaged,
 *   here. KEY AGENTS (by structural relationship): - ecumenical_patriarchate:
 *   Agenda-setting beneficiary (moderate/trapped) — articulates the claim's
 *   theological core while resident under the jurisdiction it challenges -
 *   greek_state: Primary diplomatic beneficiary (institutional/constrained) —
 *   deploys the claim bilaterally and multilaterally -
 *   eastern_orthodox_diaspora: Symbolic beneficiary
 *   (organized/identity_locked) — maintains attachment across generations -
 *   turkish_state_sovereignty: Primary target (institutional/trapped) — holds
 *   and administers the site; absorbs the standing external claim -
 *   muslim_congregations_turkey: Secondary target (organized/constrained) —
 *   present worship would end under implementation - istanbul_rum_community:
 *   Excluded local stakeholder (powerless/constrained) — bears correlated
 *   local risk - unesco_world_heritage_bodies: Analytical observer
 *   (institutional/analytical) — declines to endorse any sovereignty reading
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate: agenda_setter + beneficiary — sets the claim's doctrinal content, collects symbolic primacy, pays relationship costs inside Turkey
 *   - greek_state: beneficiary — converts the claim into diplomatic leverage and domestic coalition cement
 *   - eastern_orthodox_diaspora: beneficiary — identity anchor delivered through liturgy and commemoration, no material burden
 *   - turkish_state_sovereignty: payer — territorial arrangement permanently contested by an external normative claim it cannot extinguish
 *   - muslim_congregations_turkey: payer — congregational use interrupted again if the claim were ever realized
 *   - istanbul_rum_community: excluded — highest local exposure, least voice
 *   - unesco_world_heritage_bodies: observer — monitors conservation, refuses all sovereignty readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.36).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.14).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on Hagia Sophia (Byzantine-Origin Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural heritage/religious authority/sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '5cb7de1c-c45c-4457-bcc4-42d43d892e21').
narrative_ontology:cs_kernel_codification('5cb7de1c-c45c-4457-bcc4-42d43d892e21', formalized).
narrative_ontology:cs_authority_grounding('5cb7de1c-c45c-4457-bcc4-42d43d892e21', lineage).
narrative_ontology:cs_interpretation_layer_present('5cb7de1c-c45c-4457-bcc4-42d43d892e21').
narrative_ontology:cs_reading_relation('5cb7de1c-c45c-4457-bcc4-42d43d892e21', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5cb7de1c-c45c-4457-bcc4-42d43d892e21', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('5cb7de1c-c45c-4457-bcc4-42d43d892e21', foundational, byzantine_consecration_confers_permanent_ecclesial_title).
narrative_ontology:cs_axiom_status(byzantine_consecration_confers_permanent_ecclesial_title, holdable).
narrative_ontology:cs_axiom_grounding('5cb7de1c-c45c-4457-bcc4-42d43d892e21', byzantine_consecration_confers_permanent_ecclesial_title, theological).
narrative_ontology:cs_axiom('5cb7de1c-c45c-4457-bcc4-42d43d892e21', secondary, irretrievable_title_requires_reverent_neutral_custody).
narrative_ontology:cs_axiom_status(irretrievable_title_requires_reverent_neutral_custody, holdable).
narrative_ontology:cs_axiom_grounding('5cb7de1c-c45c-4457-bcc4-42d43d892e21', irretrievable_title_requires_reverent_neutral_custody, theological).
narrative_ontology:cs_reference_frame('5cb7de1c-c45c-4457-bcc4-42d43d892e21', byzantine_consecrated_basilica).
narrative_ontology:cs_drift_state('5cb7de1c-c45c-4457-bcc4-42d43d892e21', post_2020_reconversion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5cb7de1c-c45c-4457-bcc4-42d43d892e21', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, muslim_congregations_turkey).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_continuity_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, permanent_consecration_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Resident in Istanbul under Turkish jurisdiction, the See articulates the claim's theological core: the site was consecrated as the Great Church and that consecration is not erased by conquest. It collects symbolic primacy from the claim's persistence while paying for it — each assertion strains its legal existence inside Turkey, as the long closure of the Halki seminary illustrates. It cannot relocate the See; its stated hope is eventual return to worship, with reverent neutrality as the acknowledged fallback if restoration proves impossible.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, agenda_setter,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, beneficiary).

% Deploys the claim as a standing element of bilateral diplomacy: parliamentary resolutions, foreign-ministry statements, and advocacy in EU and US fora. It gains diplomatic leverage in disputes with Turkey and cements domestic coalitions with the Church of Greece and nationalist constituencies. Dropping the claim entirely would carry domestic political costs exceeding any bilateral friction its removal would save. It bears essentially none of the claim's local risks.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, regional).

% Millions across the Americas, Europe, and Oceania organize communal identity around the Great Church as a lost center: commemorative services each May 29, iconography, parish education, lobby organizations. The claim anchors continuity narratives across generations. Participation is liturgical and commemorative, requiring nothing material of members; attachment is constitutive of communal self-understanding rather than a chosen policy position, which is what makes departure from the claim difficult to imagine from inside.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Holds and administers the site as a state matter under presidential decree and the Directorate of Religious Affairs. It absorbs a standing external claim that its territorial arrangement is illegitimate; the claim resurfaces in every bilateral crisis, is raised in foreign legislatures, and fuels nationalist mobilization at home. It cannot relocate the site or otherwise exit the claim's object; extinguishing the claim would require concessions it classifies as sovereign surrender.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty, payer,
    institutional, generational, trapped, national).

% Worship at the site under state administration since the 2020 reconversion — Friday prayers, Ramadan observances. If the site passed to Orthodox ecclesiastical hands or were closed to worship in the name of neutrality, their congregational use would end, as it did between 1934 and 2020. Other mosques exist in abundance, but none carries this site's precedence in Ottoman endowment history; their stake is access to this specific place, and they have no seat in the forums where the claim is argued.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, muslim_congregations_turkey, payer,
    organized, generational, constrained, national).

% A few thousand Greek Orthodox remain in Istanbul, living beside the site daily. Every surge of the restitution question in bilateral politics has historically coincided with pressure on them — most catastrophically in 1955. They hold the most concrete local stake in how the question is handled and the least voice in any of the forums where it is argued; emigration remains their principal relief valve.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_rum_community, excluded,
    powerless, biographical, constrained, local).

% Monitor the site as a World Heritage property encompassing both its Byzantine and Ottoman layers. They decline to endorse any single sovereignty reading; their interventions concern conservation integrity and universal access. Their stance implicitly contests both confessional-sovereignty claims while carrying no enforcement path over the site's status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_world_heritage_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared symbolic center for geographically dispersed Orthodox communities and gives Greek diplomacy a stable symbolic position in bilateral and multilateral settings; the claim also organizes a commemorative calendar — anniversaries, memorial liturgies, educational transmission — that coordinates diaspora identity across generations.
% TRANSFER_FUNCTION: Moves symbolic capital — legitimacy, identity anchoring, diplomatic standing — toward the Patriarchate, the Greek state, and the diaspora; imposes standing sovereignty contestation and recurring diplomatic friction on Turkey; were it ever implemented, it would transfer custodianship of the site from Turkish state administration to Orthodox ecclesiastical hands and end Muslim congregational use.
% ABSENT_VOICES: Muslim congregations and the Turkish heritage administration are absent from the forums where the claim is articulated (synodal statements, Greek parliamentary resolutions, diaspora congresses); they would object that the claim writes off nearly five centuries of continuous Islamic use and present worship. The Istanbul Rum community is likewise absent — historically it has borne the sharpest local consequences whenever the claim flared, and it has the least voice anywhere.
% DISAPPEARANCE_RATIONALE: The site's material operation would continue unchanged, but a web of arrangements depends on the claim: the diaspora's commemorative calendar, the Patriarchate's symbolic-primacy narrative, Greek bilateral diplomacy's symbolic repertoire, and — symmetrically — Turkish nationalist mobilization, which feeds on the external claim as evidence of encirclement. Overnight removal would force identity and diplomatic rearrangement on every seat even though no border would move and no building would change hands.
% FOUNDING_PROBLEM: The loss of the Great Church: the 1453 conquest ended the site's life as the cathedral of the Ecumenical Patriarchate and the ceremonial center of Christian empire. The claim was built to keep Orthodox custody of that space a live question — first under Ottoman rule, later against the site's successive secular and Islamic repurposings.
% FOUNDING_PROBLEM_CORROBORATION: The condition the claim responds to — the site stands outside Orthodox custody — is publicly verifiable and effectively acknowledged by the Turkish state itself, whose officials rebut the claim rather than deny its subject matter; scholarly Byzantine studies and sustained international press coverage attest the claim's continued activity. No party outside the benefiting seats attests that the remedy (restitution or enforced neutrality) is owed: corroboration covers the problem's existence and liveness, not the cure.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.36: materially thin (no implementation pathway exists), but the claim imposes a standing sovereignty contestation that never closes — it surfaces in every bilateral crisis, is litigated in foreign legislatures, and correlates historically with pressure on Istanbul's remaining Greek Orthodox residents. Suppression is very low (0.14) because the claim possesses no coercive capacity whatsoever; it can assert, but it cannot prevent, punish, or exclude. This is why requires_active_enforcement is true while suppression stays low: the claim persists only through continuous reassertion (synodal statements, Greek diplomatic notes, diaspora commemorations) and would decay within a generation or two if unasserted — enforcement here is discursive maintenance, not coercion. Theater ratio is 0.60: a majority of the claim's observable activity is performative (May 29 commemorations, parliamentary resolutions with no operational content, anniversary protests), but a real coordination function runs beneath it — diaspora identity transmission and a stable symbolic position for Greek diplomacy. Accessibility collapse is low (0.20): alternatives are fully alive — the sibling readings operate, Turkey governs the site unilaterally, and no party's option set narrows upon understanding the claim. Resistance is high (0.70): Turkey rejects the claim outright, heritage institutions decline to endorse it, and each assertion provokes counter-mobilization. The claim/metric independence rule is honored: tangled_rope is claimed from structure (a genuine coordination function for dispersed Orthodox identity PLUS asymmetric symbolic imposition on identifiable targets PLUS active maintenance requirements), while the metrics describe observed operation independently. The measurement series run on one shared nine-point grid (1934-2025) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not tracked because the claim's coercive force has been statically negligible throughout — the scalar captures it. The extractiveness series oscillates rather than drifting monotonically: surges track external geopolitical crises (1955 pogrom era, 1964 expulsions, 1974 Cyprus, 2020 reconversion) and recede during detente (2002 EU-accession period). The oscillation is itself partly functional for the claim's holders — each crisis re-monetizes the claim domestically on BOTH sides, the Turkish side included, where the external claim serves as evidence of encirclement; this intermittent-reinforcement character is noted, though a formal cycle study would want denser points.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the turkish_state_sovereignty seat, the structure reads as an external challenge to national territory kept alive indefinitely for others' symbolic and diplomatic benefit — an experience with extraction-dominant texture, since the target is trapped (the territory cannot be moved out from under the claim) and receives nothing. From the eastern_orthodox_diaspora seat, the same structure reads as pure identity coordination — a rope-like experience in which participation costs nothing material and delivers belonging. From the ecumenical_patriarchate seat it is genuinely dual: the See both authors the claim and pays for it in legal vulnerability inside Turkey. From the analytical seat, most of the structure is inertial symbolism with episodic diplomatic utility. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the three benefiting seats near the subsidized end: the diaspora (identity_locked) sits nearest d=0 — its exit is blocked by identity fusion, deepening its structural subsidy even though the returns are purely symbolic; the Greek state (constrained) sits slightly higher, since dropping the claim would carry domestic political costs but is not unthinkable; the Patriarchate is dual-positioned — recorded as agenda_setter with secondary_role payer — because it derives symbolic primacy while absorbing relationship costs under the jurisdiction it addresses, pulling its effective directionality up from a pure-beneficiary derivation. Victim declarations place turkish_state_sovereignty near the full-target end (trapped: no exit exists from being the claim's object) and muslim_congregations_turkey high but slightly below (constrained: abundant alternative mosques exist, but none reproduces this site's endowment precedence). The istanbul_rum_community, though excluded from the conversation, sits near the target end locally — it bears the claim's sharpest correlated risks while appearing in no beneficiary ledger. UNESCO bodies sit at the analytical pole. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering, and the Patriarchate's dual position is carried structurally by its secondary role rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against the piton misread: the claim is largely performative, and a naive theater-first reading would file it as inertial residue — but concentrated beneficiaries (a state with diplomatic use for it, a diaspora with identity use for it, a See with primacy use for it) maintain it deliberately and pay real costs to do so, which is maintenance, not mere inertia. Against the snare misread: identifiable targets exist, but the claim coerces no one, suppresses no exit, and displaces no one today; its victims bear standing contestation, not dispossession. Tangled rope captures the actual hybrid: real coordination service rendered to a dispersed population, financed by symbolic imposition on a trapped neighbor. On the genealogy interview: the founding problem (the 1453 loss of the Great Church) remains live within this reading's own lights — the site stands outside Orthodox custody, which is publicly verifiable and acknowledged even by the claim's opponents — so the mandate has not outlived its function for its holders, and mandatrophy is not resolved. The obsolescence risk is asymmetric: the claim cannot die of success (implementation is unavailable) and shows no sign of dying of neglect (identity lock sustains it), making indefinite low-grade persistence the expected trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (orthodox_restitution_reading) of the hagia_sophia_substrate kernel; would instantiating a sibling reading (islamic_sovereignty_reading, universal_heritage_reading) yield a structurally different constraint with different epsilon, beneficiaries, and victims, such that cross-reading comparison rather than any single classification carries the analytic weight?',
    'Generate the sibling stories and compare computed per-seat classifications and epsilon across the constraint family; examine whether the family-level structure (mutual foreclosure between the two sovereignty readings, opportunistic citation of the heritage reading by both sides) is visible only in the reading-relations.',
    'If sibling classifications diverge sharply, the kernel''s structure lives in the reading-relations, not in any one story; treating this story alone as ''the'' Hagia Sophia constraint misclassifies the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested kernel: this story is one of three constraints sharing one substrate.').

omega_variable(
    symbolic_vs_material_extraction,
    'Is the claim''s imposition on Turkish sovereignty and Islamic worship continuity purely symbolic-diplomatic, or does it carry material components (correlated minority-security deterioration, security-posture costs, conservation interference)?',
    'Event-study correlating claim surges (1955, 1964, 1974, 2020) with material outcomes for Istanbul''s remaining Greek Orthodox residents and with the site''s operating conditions.',
    'Material confirmation would raise epsilon above the symbolic floor and sharpen the payer-seat experience toward pure extraction; purely symbolic confirmation keeps epsilon low and the arrangement predominantly identity-coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Whether the claim''s costs to its targets are symbolic only or have material channels.').

omega_variable(
    dormant_claim_activation_pathway,
    'The claim currently has no enforcement pathway; if great-power leverage comparable to 1919-1922 reappeared, would the claim convert into materially extractive displacement of Islamic worship at the site?',
    'Comparative analysis of prior activation episodes (Sevres-era provisions, the 1919-1922 occupation) against current alliance structures and enforcement vectors.',
    'An activation pathway would transform the victim set from symbolic to concrete and raise effective extraction at every seat; confirmed absence of any pathway confirms dormancy and stabilizes the low-material-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormant_claim_activation_pathway, empirical, 'Whether the claim''s practical dormancy is structural or contingent on the current balance of power.').

omega_variable(
    diaspora_identity_lock_depth,
    'Is diaspora attachment to the site identity-fused (constitutive of communal self-concept, exit unthinkable) or preference-based (capable of fading with generational turnover)?',
    'Longitudinal observation of diaspora commemorative participation across generations; test whether third-generation diaspora sustains the claim without institutional reinforcement from clergy and lobby organizations.',
    'Deep identity lock sustains the claim indefinitely at near-zero material cost, stabilizing the coordination-plus-symbolic-imposition structure; shallow lock predicts decay toward residual commemoration maintained mainly by institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_identity_lock_depth, empirical, 'Depth of the identity fusion binding the diaspora seat to the claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1934, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t1934, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1934, 0.46).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t1934, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t1955, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1955, 0.41).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t1955, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t1964, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1964, 0.43).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t1964, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t1974, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1974, 0.51).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t1974, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t1990, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1990, 0.54).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t1990, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t2002, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2002, 0.47).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t2002, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t2010, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t2010, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.64).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t2020, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_tr_t2025, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2025, 0.6).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hagia_orthodox_restitution_be_t1934, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1934, 0.15).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t1934, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t1955, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1955, 0.24).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t1955, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t1964, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1964, 0.27).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t1964, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t1974, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1974, 0.4).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t1974, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t1990, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t1990, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t2002, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2002, 0.26).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t2002, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t2010, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2010, 0.31).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t2010, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t2020, observed).
narrative_ontology:measurement(hagia_orthodox_restitution_be_t2025, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2025, 0.36).
narrative_ontology:measurement_basis(hagia_orthodox_restitution_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Hagia Sophia's legitimacy' conflates three structurally distinct claims with different epsilon values, beneficiary/victim sets, and failure modes. This story (orthodox_restitution_reading) authors the Byzantine-origin claim: low material epsilon, symbolic beneficiaries, sovereignty-contestation victims. The islamic_sovereignty_reading authors the conquest/waqf claim: currently the ENFORCED reading (state-administered since 2020), with inverted beneficiary/victim structure. The universal_heritage_reading authors the transnational claim: minimal extraction, diffuse beneficiaries, contested by both sovereignty readings. The two sovereignty readings mutually foreclose; the heritage reading coexists with both and is opportunistically cited by each side. Edges run both directions across the family: the 2020 activation of the sovereignty reading structurally intensified this reading's assertion activity (visible in the 2020 measurement surge), and each sovereignty reading's assertions supply the other's domestic mobilization fuel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
