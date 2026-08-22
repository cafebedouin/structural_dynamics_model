% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading: Jewish Survival Through Diaspora Pluralism, Not Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The diasporist reading of Jewish self-determination holds that collective
 *   survival and flourishing are secured through dispersed, culturally
 *   autonomous community life under minority-rights guarantees, and that
 *   territorial sovereignty is a dangerous deviation binding Jewish fate to a
 *   militarized state. This story instantiates that reading as a single
 *   epsilon-invariant constraint: the normative regime demanding that Jewish
 *   collective existence take diaspora-pluralist form. The regime's
 *   historical carriers — kehilla autonomy, autonomist diplomacy, the Bund —
 *   coordinated Jewish life across hostile host states for centuries; their
 *   destruction and displacement left the doctrine institutionally orphaned,
 *   surviving chiefly as academic discourse and identity rhetoric. Epsilon's
 *   referent is this standing diaspora-normative arrangement itself, assessed
 *   as it has actually operated — including its coercive communal discipline
 *   and its post-1945 atrophy — not the Zionist arrangement it condemns
 *   (which belongs to sibling stories) and not the pluralist order it
 *   endorses on paper. The claimed type and the metrics are authored
 *   independently: the reading presents its regime as benign coordination,
 *   while the metrics record coercion that decayed and theater that rose. KEY
 *   AGENTS (by structural relationship): - secular_yiddishist_networks:
 *   residual beneficiary (moderate/identity_locked) — inherits the doctrine's
 *   identity-coordination function - academic_jewish_studies_field:
 *   agenda_setter and secondary beneficiary (institutional/constrained) —
 *   administers the doctrine's contemporary meaning -
 *   liberal_diaspora_communities: incidental beneficiary (organized/mobile) -
 *   zionist_identifying_jews_in_diasporist_spaces: primary payer
 *   (powerful/mobile within governed arenas) - historical_kehilla_dissenters:
 *   historical payer (powerless/trapped) - mainstream_jewish_institutions:
 *   excluded — runs the rival sovereignty-centered arrangement -
 *   israeli_state: excluded — the standing fact the doctrine argues against -
 *   postcolonial_theory_networks: amplifier-beneficiary
 *   (institutional/mobile) - historians_of_modern_jewry: analytical observer
 *   — sees the full arc
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.24).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.1).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading: Jewish Survival Through Diaspora Pluralism, Not Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '9d68ebad-db90-4cd8-9d8e-60b96553baee').
narrative_ontology:cs_kernel_codification('9d68ebad-db90-4cd8-9d8e-60b96553baee', distributed).
narrative_ontology:cs_authority_grounding('9d68ebad-db90-4cd8-9d8e-60b96553baee', distributed).
narrative_ontology:cs_reading_relation('9d68ebad-db90-4cd8-9d8e-60b96553baee', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d68ebad-db90-4cd8-9d8e-60b96553baee', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d68ebad-db90-4cd8-9d8e-60b96553baee', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('9d68ebad-db90-4cd8-9d8e-60b96553baee', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('9d68ebad-db90-4cd8-9d8e-60b96553baee', foundational, diaspora_pluralism_secures_jewish_fate).
narrative_ontology:cs_axiom_status(diaspora_pluralism_secures_jewish_fate, holdable).
narrative_ontology:cs_axiom_grounding('9d68ebad-db90-4cd8-9d8e-60b96553baee', diaspora_pluralism_secures_jewish_fate, instrumental).
narrative_ontology:cs_axiom('9d68ebad-db90-4cd8-9d8e-60b96553baee', foundational, diaspora_is_jewish_normal_form).
narrative_ontology:cs_axiom_status(diaspora_is_jewish_normal_form, holdable).
narrative_ontology:cs_axiom_grounding('9d68ebad-db90-4cd8-9d8e-60b96553baee', diaspora_is_jewish_normal_form, deontological).
narrative_ontology:cs_reference_frame('9d68ebad-db90-4cd8-9d8e-60b96553baee', diaspora_minority_normalcy).
narrative_ontology:cs_drift_state('9d68ebad-db90-4cd8-9d8e-60b96553baee', contemporary_post_october_seventh, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d68ebad-db90-4cd8-9d8e-60b96553baee', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, secular_yiddishist_networks).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, academic_jewish_studies_field).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, liberal_diaspora_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, zionist_identifying_jews_in_diasporist_spaces).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, historical_kehilla_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, postcolonial_theory_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run Yiddish schools, cultural congresses, and heritage institutes across the diaspora on a secular, vernacular understanding of Jewish peoplehood. The doctrine gives them their charter: a Jewish future built where they live, in their language, without migration obligations. Their institutions contracted from mass movements to heritage projects over the interval; leaving the frame would mean dissolving the identity their institutions exist to carry.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, secular_yiddishist_networks, beneficiary,
    moderate, generational, identity_locked, global).

% Teaches, curates, and peer-reviews the doctrine's contemporary form: seminars on diaspora nationalism, edited volumes on autonomism, conference panels weighing sovereignty against pluralism. Careers, journals, and curricular lines are built on keeping the frame citable. Dropping it would strand syllabi and publication trajectories; deepening engagement is rewarded.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, academic_jewish_studies_field, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, academic_jewish_studies_field, beneficiary).

% Live as Jewish minorities in stable democracies, affiliated and unaffiliated. The doctrine asks nothing of them — no migration, no conscription, no collective obligations — while affirming that their communal life is a legitimate Jewish future. Most barely notice the doctrine's existence; the affirmation arrives through cultural channels rather than demands.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, liberal_diaspora_communities, beneficiary,
    organized, biographical, mobile, global).

% Jews in diaspora who identify with Israel and Zionist politics but work, study, or organize inside arenas the doctrine governs — humanities faculties, left political spaces, progressive coalitions. There they face condemnation as ethno-nationalists or collaborators, social exclusion, and professional cost. Outside those arenas they are demographically and institutionally dominant, and exiting them is easy; the price they pay is priced to the staying.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_identifying_jews_in_diasporist_spaces, payer,
    powerful, biographical, mobile, global).

% Integrators, maskilim, proto-nationalists, and families of converts who deviated from communal discipline in the centuries before emancipation. Communal authorities could fine, ban, excommunicate, or sever them from marriage, burial, and commerce; host states offered no protection and no alternative Jewish collective form.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, historical_kehilla_dissenters, payer,
    powerless, immediate, trapped, regional).

% Federations, defense agencies, synagogue movements, and philanthropies that organize diaspora Jewish life around Israel-centrism: missions, fundraising, advocacy, education. They reject the doctrine's premises outright and would denounce them from any seat offered; they are absent from the doctrine's councils because its carriers long ago stopped seeking their assent.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, mainstream_jewish_institutions, excluded,
    institutional, generational, arbitrage, global).

% Exists as the standing fact the doctrine argues against: a sovereign Jewish state with a conscript army and a Law of Return that relocated the center of Jewish gravity. It neither seeks nor needs a seat in the doctrine's framework; its officials dismiss the doctrine as diaspora self-indulgence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, israeli_state, excluded,
    institutional, generational, arbitrage, national).

% Political theorists, sociologists, and area-studies networks beyond Jewish studies who take the Jewish case as confirming evidence for general claims about nationalism's dangers and minority flourishing. Amplifying the doctrine extends their theoretical portfolio; they hold no stake in Jewish communal outcomes.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, postcolonial_theory_networks, beneficiary,
    institutional, biographical, mobile, global).

% Academic historians studying how the doctrine actually operated — kehilla finances, treaty diplomacy, Bund politics, the destruction of the European centers. Their archival base sits outside any holder's commitments; their assessments are cited by all sides and owned by none.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, historians_of_modern_jewry, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustained a single Jewish collective life across mutually hostile host polities without territorial claims: standardized halakhic practice, transnational charity and scholarly circuits, negotiated communal autonomy in each host state, and — in its modern phase — minority-rights diplomacy to win collective cultural guarantees from multinational empires and new nation-states.
% TRANSFER_FUNCTION: Moves dues, taxes, and loyalty commitments from individual Jews to transnational communal institutions (kehillot, the councils of the Four Lands, later the Bund and YIVO), and moves security responsibility outward — from Jewish hands to host-state toleration, treaty guarantees, and majority goodwill. In its contemporary academic form it moves interpretive jurisdiction over 'authentic Jewishness' to diaspora-normative intellectuals.
% ABSENT_VOICES: During the doctrine's ascendancy: Jews pursuing sovereignty (Hibbat Zion, Herzl's circle) were condemned or pushed out of communal institutions and absent from its councils. After 1948, Mizrahi Jews expelled from Arab states — whose trajectory falsified the minority-rights wager — remained largely outside its canonical statements. Throughout, Palestinian Arabs are absent from every seat of this reading's framework. They sit outside the doctrine's discursive institutions: the Bundist, rabbinic-academic, and postcolonial spaces that produce its statements.
% DISAPPEARANCE_RATIONALE: A narrow rearrangement. The academic subfields, activist identity projects, and Yiddishist cultural programs organized around the doctrine would lose their charter and reorganize; anti-Zionist Jewish politics would need a new warrant. The security architecture of Jewish life — state sovereignty, diaspora citizenship regimes — would not change, because the doctrine no longer governs it. The rearrangement is confined to the discursive and identity arrangements of its remaining holders.
% FOUNDING_PROBLEM: How can a geographically dispersed minority sustain collective law, language, and solidarity across generations under host states that condition toleration on political quietism — collective survival without sovereignty?
% FOUNDING_PROBLEM_CORROBORATION: The problem's historical reality is corroborated from outside the beneficiary set: imperial Russian, Austro-Hungarian, and Ottoman administrative archives; the historiography of East-Central European Jewry, written by historians with no stake in the doctrine's revival; and, from an independent theological grounding, the Haredi three-oaths tradition, which attests the anti-sovereignty norm without sharing the doctrine's secular premises. Its status as dead is attested by the changed conditions — sovereignty exists and is defended, and diaspora Jewry thrives under citizenship regimes — and by mainstream Zionist and Israeli institutional historiography. Segments of the antisemitism-monitoring community and the doctrine's own holders argue the problem recurs under renewed conditions; both stand inside or adjacent to the contest. No disinterested party currently attests the founding problem as live in its original form, and that absence is itself signal.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).
:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness declines across the interval (0.46 to 0.24) because the machinery that made the regime extractive — kehilla taxation, disciplinary bans, marriage-and-commerce leverage — was dismantled by emancipation, destroyed in the catastrophe of 1939-1945, and displaced by the sovereignty-centered arrangement; what extraction remains is reputational and confined to the doctrine's governed arenas, with a wartime spike at 1939 when failing authorities extracted desperately from shrinking populations. Theater rises steeply (0.10 to 0.82) as function drained out and performance filled the vessel: the doctrine's contemporary activity is seminars, anthologies, commemorations, and coalition rhetoric rather than governance of anything. The suppression_requirement series is authored deliberately as a falling trajectory because the story's dynamic is enforcement-capacity decay — from host-state-backed communal coercion (0.70) down to purely discursive sanction (0.10) — not a static suppression picture. Suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream by directionality and scope in the engine's computation. Accessibility_collapse is low (0.15) because nothing collapses when the doctrine is understood: Zionist, covenantal, and liberal-nationalist alternatives remain fully available — indeed dominant. Resistance is high (0.72) because nearly the entire organized Jewish world actively rejects the doctrine. All three series share one eight-point time grid; every metric is authored at every point, with no substitution of end-state values into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge sharply. The yiddishist seat experiences the doctrine as inheritance and identity-substance — near-beneficiary, identity-locked into its benefits. The academic agenda-setter seat experiences it as a living analytic program and would report low theater from inside the seminar room. The liberal-diaspora seat registers it as background affirmation it never asked for. The payer seats — Zionist-identifying Jews inside governed arenas, and the historical dissenters before them — experienced condemnation, exclusion, or communal ruin. Because the contemporary payer exit is mobile rather than trapped, computed extraction for that seat is damped relative to trapped targets: the doctrine extracts most fiercely precisely where exit is costliest, which today is almost nowhere. The engine computes these per-seat classifications from the structural data; the divergence between the reading's benign self-presentation and the payer-seat experience is the measurement this story contributes, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: secular_yiddishist_networks (direct, identity-locked — low d, locked into benefit), academic_jewish_studies_field (agenda-setter and collector of career goods — low-to-moderate d), postcolonial_theory_networks (amplifier-beneficiary), liberal_diaspora_communities (incidental, mobile — damped toward symmetry). Payers: zionist_identifying_jews_in_diasporist_spaces (high d within governed arenas, damped by mobility) and historical_kehilla_dissenters (maximal d in their era: powerless and trapped). Spatial scope is global — the doctrine claims the whole diaspora — which amplifies whatever effective extraction survives. Receipt surface: gain_flow is authored as diffuse as an affirmative, seat-checked claim — the yiddishist seat receives identity goods, the academic seat career goods, but the costs imposed on payers accrue to no seat's balance sheet, and no seat captures the doctrine's remaining extraction as revenue or standing convertible to control. fixing_cost is prohibitive: no seat bears enough of the doctrine's cost to fund its removal, and removal would require coordinating abandonment across dispersed holders none of whom is harmed enough to lead it — the cost-asymmetry of an administratively orphaned arrangement, not the entrenchment of a captured one.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification prevents two symmetric mislabels. Read charitably, the doctrine looks like a live coordination mechanism still organizing diaspora identity; read cynically, like a snare feeding academic careers. Both misread the structure: the coordination function that once justified the regime died with its conditions, and the rents it once collected died with its enforcement machinery — what persists is maintenance without function, kept alive by institutional inertia and the identity investments of its remaining holders. The R5 interview records the signature directly: founding_problem_status dead paired with disappearance_verdict world_rearranges. Arrangements still depend on it, but only discursive and identity arrangements, not the security architecture it was built to manage. That mismatch is the zombie flag; the consumer should cross-check it against the theater trajectory, which dates the functional death to 1939-1948 and the theatrical consolidation to the decades after.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This constraint is one reading of the contested kernel jewish_self_determination — the diasporist_reading. Which structural elements do the sibling readings relocate?',
    'Cross-reading comparison within the kernel family: the liberal_nationalist and indigenous_return siblings move the beneficiary set to state-institution constituencies; the settler_colonial sibling moves the victim set to displaced Palestinians; the religious_covenant sibling replaces consequentialist grounding with covenantal obligation.',
    'Classification is reading-indexed: the same kernel yields different types, beneficiary sets, and victim sets under each sibling. Only the victim-set and grounding differences distinguish them; averaging across readings would destroy the epsilon invariance this story preserves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: this story instantiates one of five readings of the self-determination kernel; committer content routed here per Rule 2.').

omega_variable(
    falsification_vs_contingency,
    'How much of the doctrine''s post-1945 discredit reflects falsified premises (minority-rights frameworks cannot secure a despised minority) versus contingent catastrophe (no available strategy would have saved European Jewry)?',
    'Comparative counterfactual historiography of 1933-1945 strategic options, and systematic comparison of minority-treaty regime performance against sovereignty performance for comparable minorities across the twentieth century.',
    'If falsification dominates, the drift_state magnitude moves toward severe and founding_problem_status stays dead. If contingency dominates, the frame''s premises retain live warrant and the reading regains analytic standing despite the historical record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(falsification_vs_contingency, empirical, 'Whether the doctrine failed on its merits or against an unanswerable environment.').

omega_variable(
    hegemonic_displacement_vs_exhaustion,
    'Is the doctrine''s atrophy caused by active displacement (a Zionist monopoly on the definition of ''Jewish interest'' crowding out alternatives and absorbing their institutions and funding) or by intrinsic exhaustion (demographic integration, the falsified security promise, the death of its carriers)?',
    'Institutional histories tracing funding flows, organizational absorption, and discursive displacement of Bundist and autonomist institutions between 1945 and 1990, distinguishing crowding-out from voluntary abandonment.',
    'If displacement dominates, the sibling Zionist-hegemony arrangement computes with an active suppression component toward this doctrine''s holders, and this story''s atrophy is partly exogenous; if exhaustion dominates, the piton reading stands unmodified as an endogenous decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_displacement_vs_exhaustion, empirical, 'Whether the atrophied state was inflicted by a rival arrangement or reached on its own.').

omega_variable(
    beneficiary_reality_check,
    'Do liberal diaspora communities genuinely benefit from the doctrine, or is their recorded benefit an analytic projection by the academic seat that studies them?',
    'Attitudinal and behavioral data: whether non-affiliated and liberal diaspora Jews'' communal choices track the doctrine''s commitments in any detectable way, or proceed indifferent to them.',
    'If projection, the beneficiary set contracts to the yiddishist and academic seats; the theater_ratio interpretation shifts toward pure self-maintenance by the doctrine''s carriers, strengthening the piton reading and weakening the residual coordination-function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_reality_check, conceptual, 'Whether the declared beneficiary structure is real or analyst-manufactured.').

omega_variable(
    post_2023_security_premise,
    'Does the post-October-2023 surge in antisemitic violence revive the founding problem (moving founding_problem_status toward contested or live) or further fossilize the doctrine as academic artifact?',
    'Longitudinal survey of diaspora Jewish institutional alignments, aliyah statistics, and security-driven organizational restructuring, tracked against doctrine-adherence indicators over the coming decade.',
    'A revival flips the founding_problem_status x disappearance_verdict pairing away from the piton signature and may date a transition out of the current atrophied state; continued fossilization hardens the current classification and extends the theater trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2023_security_premise, empirical, 'Forward-looking uncertainty on whether renewed insecurity resurrects the doctrine''s founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_self_determination__diasporist_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__diasporist_reading, theater_ratio, 1897, 0.12).
narrative_ontology:measurement(jewi_tr_t1919, jewish_self_determination__diasporist_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(jewi_tr_t1939, jewish_self_determination__diasporist_reading, theater_ratio, 1939, 0.3).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.55).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.62).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__diasporist_reading, theater_ratio, 1990, 0.72).
narrative_ontology:measurement(jewi_tr_t2025, jewish_self_determination__diasporist_reading, theater_ratio, 2025, 0.82).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_self_determination__diasporist_reading, base_extractiveness, 1880, 0.46).
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__diasporist_reading, base_extractiveness, 1897, 0.44).
narrative_ontology:measurement(jewi_be_t1919, jewish_self_determination__diasporist_reading, base_extractiveness, 1919, 0.38).
narrative_ontology:measurement(jewi_be_t1939, jewish_self_determination__diasporist_reading, base_extractiveness, 1939, 0.52).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.34).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__diasporist_reading, base_extractiveness, 1990, 0.26).
narrative_ontology:measurement(jewi_be_t2025, jewish_self_determination__diasporist_reading, base_extractiveness, 2025, 0.24).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_self_determination__diasporist_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(jewi_su_t1897, jewish_self_determination__diasporist_reading, suppression_requirement, 1897, 0.68).
narrative_ontology:measurement(jewi_su_t1919, jewish_self_determination__diasporist_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(jewi_su_t1939, jewish_self_determination__diasporist_reading, suppression_requirement, 1939, 0.5).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(jewi_su_t1990, jewish_self_determination__diasporist_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(jewi_su_t2025, jewish_self_determination__diasporist_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'Jewish self-determination' into five epsilon-invariant readings: this file is the diasporist reading; the liberal_nationalist, indigenous_return, settler_colonial, and religious_covenant siblings instantiate the same kernel with different beneficiary sets, victim sets, and groundings. The diasporist reading sits upstream of the settler_colonial sibling — its internal-Jewish warrant supplies legitimacy that the dispossession critique cites — and presupposes the liberal_nationalist sibling as the baseline it negates. Epsilon differs across members because each reading measures a different standing arrangement; conflating them would violate epsilon invariance. Every member links the others through network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
