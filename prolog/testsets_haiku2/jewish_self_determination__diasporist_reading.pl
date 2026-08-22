% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Jewish Diaspora Pluralism as Path to Survival (Diasporist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the diasporist reading of the contested
 *   kernel 'Jewish self-determination.' It asserts that Jewish collective
 *   survival and flourishing are best secured through diaspora pluralism and
 *   minority-rights frameworks, not territorial sovereignty; that Zionism
 *   represents a dangerous deviation tying Jewish fate to a militarized
 *   state. The reading emerges from early-modern Enlightenment premises (that
 *   minority status within pluralist societies is sustainable and dignifying)
 *   and constructs a normative alternative to 20th-century Zionist hegemony
 *   over Jewish institutional life. The constraint measures as piton:
 *   diaspora institutions that once expressed this reading have atrophied,
 *   suppressed by Zionist institutional dominance; what remains of diasporist
 *   practice is largely theatrical — Holocaust commemoration ceremonies,
 *   cultural events — without the institutional substrate that once animated
 *   it. The reading does not itself specify the mechanisms of institutional
 *   suppression (structural exclusion vs. internalized identity costs); that
 *   ambiguity is addressed through omega variables. Importantly, this
 *   constraint brackets Palestinian dispossession as a separate question;
 *   Palestinian voices and anti-colonial frames are excluded from the
 *   reading's discourse. The diasporist reading thus instantiates a
 *   minority-rights framework that may itself be complicit with
 *   settler-colonial arrangements, a structural irony not resolved within the
 *   reading itself.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: maintain collective identity through minority-status institutions and cultural networks; benefit from diasporist framing.
 *   - zionist_institutional_apparatus: agenda-setter that frames 'Jewish interest' as territorial sovereignty; monopolizes diaspora institutional resources.
 *   - jews_coerced_into_zionist_framework: identity-locked victims bearing the cost of enforced alignment; exit is psychologically costly.
 *   - jews_endangered_by_israeli_state_actions: trapped victims whose standing is jeopardized by association with Israeli military conduct.
 *   - palestinian_communities: excluded from diasporist discourse; would contest the decoupling of Jewish security from Palestinian subjugation.
 *   - antisemitic_state_and_nonstate_actors: weaponize both diasporist and Zionist frames; structurally excluded from the reading's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.62).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Jewish Diaspora Pluralism as Path to Survival (Diasporist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a').
narrative_ontology:cs_kernel_codification('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', distributed).
narrative_ontology:cs_authority_grounding('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', distributed).
narrative_ontology:cs_reading_relation('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', foundational, minority_status_sustainable_survival_path).
narrative_ontology:cs_axiom_status(minority_status_sustainable_survival_path, holdable).
narrative_ontology:cs_axiom_grounding('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', minority_status_sustainable_survival_path, empirically_contingent).
narrative_ontology:cs_axiom('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', foundational, territorial_sovereignty_unnecessary_for_jewish_flourishing).
narrative_ontology:cs_axiom_status(territorial_sovereignty_unnecessary_for_jewish_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', territorial_sovereignty_unnecessary_for_jewish_flourishing, empirically_contingent).
narrative_ontology:cs_reference_frame('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', enlightenment_pluralist_framework).
narrative_ontology:cs_drift_state('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', post_1948_zionist_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('08b3fdfb-49f6-4cb7-a014-76a7ddd7c61a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_actions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities in North America, Europe, and elsewhere maintain distinct cultural and religious practices within pluralist host societies. They benefit from framing Jewish identity as compatible with minority status, institutional autonomy, and non-territorial belonging. They administer diaspora institutions (community centers, educational networks, mutual-aid organizations) that express this framework. Their exit option is passive — diaspora institutions have attenuated; active reassertion would require rebuilding cultural infrastructure and competing against Zionist institutional dominance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, agenda_setter).

% Jews who experience pressure (social, institutional, familial) to identify with Israeli state interests or Zionist ideology as a condition of Jewish belonging. They bear the cost of alignment when Israeli state actions generate international criticism or when Zionist framing constrains how they can express dissent. Their exit is identity-locked: rejecting Zionism risks social isolation within Jewish institutional spaces or mischaracterization as self-hating.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    moderate, biographical, identity_locked, global).

% Jews whose security or standing is jeopardized by association with Israeli military action, occupation, or settlement policy. In countries where antisemitism is weaponized against all Jews via Israeli state conduct, they carry the cost without influence over state policy. Their exit is trapped: they cannot exit Jewishness; they cannot exit association with the state's actions in hostile environments.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_actions, payer,
    powerless, biographical, trapped, global).

% Jewish organizations, federations, and networks that frame Jewish survival as contingent on Israeli state security and territorial sovereignty. They set the terms of 'Jewish interest,' mobilize diaspora resources toward Israeli support, and marginalize or exclude diasporist and anti-Zionist voices from institutional platforms. They exercise exit through arbitrage: they redirect diaspora institutional resources and allegiance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Palestinians whose claims to land, return, and self-determination are structurally excluded from diasporist Jewish discourse. The diasporist reading brackets Palestinian dispossession as a separate question; Palestinian voices are not in the room where the constraint's legitimacy is debated. They would challenge the frame that decouples Jewish security from Palestinian subjugation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, palestinian_communities, excluded,
    powerless, generational, trapped, global).

% Governments in diaspora countries that tolerate or support Jewish minority communities and their institutions. They observe the constraint as a question of whether Jewish collective flourishing depends on territorial sovereignty elsewhere or on secure minority status within pluralist frameworks. Their analytical seat is distant from the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_governments, observer,
    institutional, generational, analytical, national).

% Actors whose interest lies in depicting Jews as foreign, disloyal, or a security threat to host societies. They weaponize both diasporist and Zionist framings: diasporism as proof of Jewish non-belonging, Zionism as proof of dual loyalty. Their exclusion from the constraint's discourse is structural — the reading does not admit their epistemic standing. They would dispute both readings' legitimacy from a position of existential threat.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, antisemitic_state_and_nonstate_actors, excluded,
    powerful, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, zionist_institutional_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes Jewish collective identity and survival strategy around minority-rights protections, cultural autonomy within pluralist host societies, and international human-rights frameworks, rather than territorial sovereignty. Solves the coordination problem of how dispersed Jewish communities maintain cultural continuity and mutual support without a state apparatus.
% TRANSFER_FUNCTION: Diasporist institutions and discourse transfer resource, attention, and legitimacy from Zionist territorial frameworks toward diaspora cultural, educational, and mutual-aid infrastructure. It also transfers burden: Jews who maintain diasporist identity must continuously argue against institutional Zionist hegemony; they bear the friction cost of institutional dissent.
% ABSENT_VOICES: Palestinians whose dispossession is the material ground of Zionist settlement are structurally absent from diasporist discourse — the reading brackets that history as a separate question. Anti-Zionist Jewish voices within Israel are also marginalized by diaspora diasporist frameworks that treat Zionism as an external deviation rather than an internal Jewish political movement with deep historical roots. Jews in Israel who experience both Zionist settler identity and Palestinian dispossession as inseparable are not in the room.
% DISAPPEARANCE_RATIONALE: If diasporist institutional frameworks and discourse suddenly vanished, diaspora Jewish communities would not cease to exist, but their capacity for collective self-organization outside Zionist frameworks would collapse to near-zero. Zionist institutional dominance is already high; the disappearance would accelerate it. The diasporist reading interprets this as catastrophic loss of Jewish cultural autonomy. The Zionist reading interprets it as necessary concentration of Jewish resources toward territorial security. The disappearance verdict is contested because the parties disagree on whether diaspora community survival is viable.
% FOUNDING_PROBLEM: Jewish dispersal across multiple nation-states and the precariousness of minority status in societies subject to antisemitic violence created the problem of how to sustain Jewish collective identity and survival without territorial concentration. Diasporist frameworks answer: through international minority-rights protections, cultural-religious autonomy, and networks of diaspora institutions. Early-modern and modern Enlightenment frameworks that promised toleration and equal citizenship offered this path as viable.
% FOUNDING_PROBLEM_CORROBORATION: Diasporist Jewish intellectuals and historians (e.g., historians of diaspora Judaism, scholars of Jewish minority institutions in pre-1948 Europe and the Americas) attest the founding problem remains live: diaspora Jewish communities flourish culturally and institutionally in pluralist societies without territorial sovereignty. Zionist and indigenous-return readings attest the founding problem is dead: repeated waves of persecution culminating in the Holocaust proved minority status unsustainable, making territorial sovereignty necessary. Palestinian scholars and postcolonial theorists outside the Jewish community attest a different problem entirely: the diasporist reading systematizes Jewish minority protection at the cost of Palestinian dispossession, which is not addressed as a problem within the reading itself.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint's operation depends on suppressing diasporist alternatives in favor of Zionist institutional dominance — a transfer of legitimacy, resources, and institutional authority from diaspora frameworks to territorial sovereignty frameworks. Suppression is high (0.71) because active enforcement is required to maintain Zionist hegemony: diasporist voices must be marginalized, excluded, or absorbed into Zionist frames; diaspora institutions must be reoriented toward Israeli support. Theater ratio is elevated (0.58) because much of what remains of diasporist institutional life is performative — cultural events, Holocaust commemoration — without the institutional capacity that once sustained alternative Jewish futures. The trajectory is one of institutional atrophy with increasing theatricality: the constraint persists not because diasporist institutions command resources or because the agents who would maintain them actively defend the arrangement, but because inertia, identity fusion, and institutional path-dependence keep the form alive while the function decays. Accessibility collapse is relatively low (0.48) because alternatives (Zionism, religious covenant, liberal nationalism) are visible and actively pursued; the diasporist reading remains conceptually available even as institutional expression is suppressed. Resistance is high (0.73) because diasporist intellectuals, historians, and activists continue to contest Zionist hegemony; the suppression is sustained against active dissent. One shared temporal grid underlies all three metric series: measurement dates represent moments of institutional and political change — the founding of Israel (t=0), the 1967 war and occupation (t~12), the 1982 Lebanon invasion (t~25), the First Intifada and Palestinian uprising (t~38), the Oslo Accords and their collapse (t~50), and the contemporary moment of Israeli-Palestinian crisis (t~63–75).
 *
 * PERSPECTIVAL GAP:
 *   The diaspora Jewish communities seat (beneficiary/agenda-setter) perceives the constraint as a protective frame for Jewish cultural autonomy and minority flourishing; they experience Zionist hegemony as imposition. The Zionist institutional apparatus seat (agenda-setter) perceives the same institutional structure as necessary concentration of Jewish resources toward territorial security; they experience diasporist resistance as dangerous dilution of Jewish power. The identity-locked victims (Jews coerced into Zionism) perceive the constraint through the lens of institutional pressure: they experience both diasporism (as a suppressed alternative they might claim) and Zionism (as the enforced default) as external impositions on their agency. The trapped victims (Jews endangered by Israeli actions) perceive the constraint as a structure that ties their social standing to a militarized state they cannot influence. The excluded Palestinian seat would perceive both diasporism and Zionism as frameworks that erase Palestinian dispossession and Palestinian Jewish-Muslim coexistence. These divergent perceptions are not reconciled — the engine computes per-seat type-classifications from the structural data; the author's task is to state the perspectival gap clearly.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities benefit from diasporist framing without directly running the constraint (they administer diaspora institutions but do not control the broader frame); directionality approaches 0.25–0.35 (moderate beneficiary, not full). The Zionist institutional apparatus sets the dominant frame and benefits from resource concentration; directionality approaches 0.10–0.20 (beneficiary). Jews coerced into Zionism experience identity-lock (cannot exit without risking social isolation within Jewish spaces) and bear the cost of alignment; directionality approaches 0.65–0.75 (substantial target, somewhat trapped). Jews endangered by Israeli actions are powerless and trapped; directionality approaches 0.85–0.95 (near-full target). Directionality overrides are not needed here: the derivation from beneficiary/victim + exit + power yields the right asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint presents a mandatrophy signature: the founding problem (Jewish survival without territorial sovereignty) is contested — diasporists claim it remains live and is solved by pluralist frameworks; Zionists and others claim it is dead and disproved by the Holocaust. The disappearance verdict is similarly contested: diasporists argue that diaspora institutional capacity would collapse; Zionists argue that Zionist institutions would simply continue. This unresolved mandatrophy is NOT a defect in the constraint's classification — it is a structural feature of the reading itself. Mandatrophy is the space where the kernel-reading structure lives. The classification as piton (not snare, not rope) depends on recognizing that the constraint persists not because agents are benefiting enough to defend it or being extracted from enough to fix it, but because institutional inertia and identity-fusion hold the form while the function atrophies. The distinction is crucial: a snare would show active extractive agents defending the arrangement; a piton shows mostly performance, with no concentrated agent profiting and no concentrated agent mobilized to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is diasporism a reading of a contested kernel (Jewish self-determination), or is it a distinct constraint that competes with Zionism as alternative frameworks?',
    'Genealogical and doctrinal analysis: do diasporist thinkers frame their position as interpretation of Jewish self-determination claims, or as rejection of the self-determination frame itself? Do they appeal to a shared text, tradition, or covenant, or do they construct an alternative epistemic ground?',
    'If reading: diasporism and Zionism both instantiate the kernel ''Jewish self-determination'' and differ on how to interpret it. If alternative frame: diasporism rejects the self-determination premise entirely and grounds survival in minority-rights frameworks external to Jewish tradition. This affects whether the engine treats them as rival readings or as logically disjoint constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether diasporism is a reading of Jewish self-determination or a rejection of the framework.').

omega_variable(
    institutional_atrophy_causation,
    'Are diaspora Jewish institutions attenuated because of Zionist institutional hegemony (the piton hypothesis), or because modern pluralist societies have naturally reduced the need for ethnic-minority institutional infrastructure?',
    'Comparative institutional analysis: examine the trajectory of diaspora institutions in societies with strong minority-rights protections vs. weak ones; examine the resource-allocation patterns of major Jewish federations before and after institutional Zionist consolidation; conduct oral histories with diaspora institutional practitioners about experienced pressure.',
    'If causation is Zionist: the constraint is indeed piton — an atrophied function maintained theatrically by suppressing alternatives. If causation is secular modernization: the constraint is better classified as scaffold (transitional arrangement now obsolete) or rope (a coordination function that served its purpose and naturally declined).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_atrophy_causation, empirical, 'Why diaspora institutions have weakened: Zionist hegemony or secular modernization?').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of diasporist alternatives structural (institutional exclusion, resource denial, regulatory barriers) or internalized (diaspora Jews internalize Zionist frames as inevitable, making exit psychologically costly)?',
    'Post-exit trajectory analysis: study diaspora Jews who exit or reject Zionist institutional frames. Do they report structural barriers (exclusion, job loss, family rupture) or internalized barriers (shame, guilt, identity confusion)? Both? Ethnographic documentation of institutional decision-making about diasporist voices.',
    'If structural: suppression is a raw institutional property; removing institutional gatekeeping would rapidly de-suppress alternatives. If internalized: the constraint carries the suppression with it even after institutional reform; it would require re-enculturation and identity work to lift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Is suppression structural, internalized, or both?').

omega_variable(
    diaspora_viability_empirical_claim,
    'Can Jewish collective survival and flourishing sustainably persist without territorial sovereignty, relying on minority-rights protections and cultural autonomy within host societies?',
    'Long-term empirical tracking: assess the vitality of diaspora Jewish communities (cultural production, institutional activity, intergenerational transmission, subjective well-being) in different national contexts over 50+ year timescales. Compare with outcomes in Zionist-aligned communities and Israel. Control for antisemitism, war, and host-state stability.',
    'If diaspora viability is high: the diasporist claim is empirically vindicated; Zionism is a deviation from a workable path. If viability is low: the founding problem is indeed dead; territorial sovereignty becomes necessary. If outcomes diverge by national context: both claims are context-dependent, and universalizing either is false.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_viability_empirical_claim, empirical, 'Is diaspora-based Jewish survival empirically viable long-term?').

omega_variable(
    kernel_reading_sibling_relations,
    'What are the logical and structural relationships between this diasporist reading and the sibling readings of Jewish self-determination?',
    'Committer-axis analysis: map the core normative premises of each reading; identify points of direct logical contradiction, structural pressure points, and frameworks where multiple readings could coexist.',
    'Determines whether sibling readings foreclose diasporism, coexist with it, or influence it structurally. This shapes the engine''s classification of each reading within the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relations, conceptual, 'Logical and structural relationships to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jewi_tr_t12, jewish_self_determination__diasporist_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__diasporist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(jewi_tr_t38, jewish_self_determination__diasporist_reading, theater_ratio, 38, 0.48).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__diasporist_reading, theater_ratio, 50, 0.54).
narrative_ontology:measurement(jewi_tr_t63, jewish_self_determination__diasporist_reading, theater_ratio, 63, 0.57).
narrative_ontology:measurement(jewi_tr_t75, jewish_self_determination__diasporist_reading, theater_ratio, 75, 0.58).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t12, jewish_self_determination__diasporist_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__diasporist_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(jewi_be_t38, jewish_self_determination__diasporist_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__diasporist_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(jewi_be_t63, jewish_self_determination__diasporist_reading, base_extractiveness, 63, 0.62).
narrative_ontology:measurement(jewi_be_t75, jewish_self_determination__diasporist_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__diasporist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jewi_su_t12, jewish_self_determination__diasporist_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__diasporist_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(jewi_su_t38, jewish_self_determination__diasporist_reading, suppression_requirement, 38, 0.64).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__diasporist_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(jewi_su_t63, jewish_self_determination__diasporist_reading, suppression_requirement, 63, 0.7).
narrative_ontology:measurement(jewi_su_t75, jewish_self_determination__diasporist_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the kernel 'Jewish self-determination.' Each reading instantiates a different constraint with distinct epsilon, beneficiary/victim structure, and type. The diasporist reading emphasizes minority-rights frameworks and institutional autonomy; it is linked to sibling readings through logical relationships (forecloses, coexists_with, influences) that determine how one reading's adoption affects the viability of others. The constraint family represents the contested terrain of Jewish political philosophy post-1948; no single reading monopolizes 'correct' interpretation of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
