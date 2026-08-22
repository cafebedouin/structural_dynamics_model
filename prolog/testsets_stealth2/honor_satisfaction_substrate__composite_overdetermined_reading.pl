% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor-Satisfaction Substrate — Composite Overdetermined Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The honor-satisfaction substrate — the gentlemanly obligation to answer a
 *   perceived insult with ritualized single combat under codified rules —
 *   governed dispute resolution inside European officer corps and gentry
 *   society from the early modern period into the nineteenth century. This
 *   story instantiates the composite overdetermined reading of the contested
 *   kernel honor_satisfaction_substrate: the arrangement's dissolution was
 *   driven simultaneously by exogenous suppression (statutory prohibition,
 *   army discipline codes, honor-court reform) and endogenous delegitimation
 *   (the transformation of honor-centered into dignity-centered elite
 *   selfhood), with the two causal pathways non-independent — prohibition
 *   raised the price of compliance for men still bound to the code,
 *   accelerating the credibility collapse of the honor frame, while the
 *   frame's erosion lowered the political cost of prohibition. The epsilon
 *   referent is the standing arrangement itself — the operative
 *   honor-satisfaction system across its decline interval (t=0 approximates
 *   1780, t=80 approximates 1860) — assessed by this reading's own lights: a
 *   structure with a genuine violence-channeling coordination function AND
 *   compelled-participant extraction, whose decay passes through an
 *   enforcement-ratchet phase before settling into ceremonial remnant.
 *   Claim/metric independence is preserved deliberately: claimed_type names
 *   the arrangement's operative-life structure, while the end-state metrics
 *   describe the vestigial remnant the interval closes on; that divergence is
 *   the lifecycle datum, not an error. KEY AGENTS (by structural
 *   relationship): - regimental_officer_establishment: Agenda-setting
 *   administrator (institutional/mobile) — runs honor courts, converts the
 *   code into command authority, collects the deference dividend -
 *   established_gentry_lineages: Primary beneficiary
 *   (powerful/identity_locked) — honor capital anchors class closure;
 *   identity fused with the honor order - dueling_professionals: Secondary
 *   beneficiary (moderate/mobile) — sells arms, instruction, seconds'
 *   service, surgical attendance into the affair economy -
 *   junior_officers_compelled_to_fight: Primary target
 *   (moderate/identity_locked) — bears compelled mortal risk; refusal means
 *   professional and social death - duel_casualties_and_bereaved:
 *   Realized-harm bearer (powerless/trapped) — the killed, maimed, widowed,
 *   orphaned; no seat in any proceeding that produced them -
 *   refusing_gentlemen: Enforcement-display bearer (moderate/trapped) —
 *   absorbs ostracism and career termination that teach the rest compliance -
 *   state_legal_apparatus: Counter-agenda setter (institutional/mobile) —
 *   legislates and prosecutes prohibition; consolidates the violence monopoly
 *   as the code recedes - anti_dueling_reformers: Excluded objectors
 *   (organized/constrained) — clerical and utilitarian campaigners with no
 *   seat in the fora where affairs of honor are adjudicated
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.41).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor-Satisfaction Substrate — Composite Overdetermined Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '7cca9ea3-58db-4055-b91e-2722a3151297').
narrative_ontology:cs_kernel_codification('7cca9ea3-58db-4055-b91e-2722a3151297', formalized).
narrative_ontology:cs_authority_grounding('7cca9ea3-58db-4055-b91e-2722a3151297', practice).
narrative_ontology:cs_interpretation_layer_present('7cca9ea3-58db-4055-b91e-2722a3151297').
narrative_ontology:cs_reading_relation('7cca9ea3-58db-4055-b91e-2722a3151297', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('7cca9ea3-58db-4055-b91e-2722a3151297', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('7cca9ea3-58db-4055-b91e-2722a3151297', foundational, causal_pathway_nonindependence).
narrative_ontology:cs_axiom_status(causal_pathway_nonindependence, holdable).
narrative_ontology:cs_axiom_grounding('7cca9ea3-58db-4055-b91e-2722a3151297', causal_pathway_nonindependence, empirically_contingent).
narrative_ontology:cs_axiom('7cca9ea3-58db-4055-b91e-2722a3151297', foundational, mechanism_simultaneity_not_sequence).
narrative_ontology:cs_axiom_status(mechanism_simultaneity_not_sequence, holdable).
narrative_ontology:cs_axiom_grounding('7cca9ea3-58db-4055-b91e-2722a3151297', mechanism_simultaneity_not_sequence, empirically_contingent).
narrative_ontology:cs_reference_frame('7cca9ea3-58db-4055-b91e-2722a3151297', dual_track_honor_legality_order).
narrative_ontology:cs_drift_state('7cca9ea3-58db-4055-b91e-2722a3151297', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7cca9ea3-58db-4055-b91e-2722a3151297', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, established_gentry_lineages).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, regimental_officer_establishment).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_professionals).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, junior_officers_compelled_to_fight).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, duel_casualties_and_bereaved).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, refusing_gentlemen).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, code_duello_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, personal_honor_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs honor courts and regimental opinion, adjudicates affairs of honor, and decides which apologies are acceptable and which refusals punishable. Converts the code into command authority: juniors who cannot afford a challenge or a refusal remain deferential. Bears the administrative cost of enforcement and collects the deference dividend; can reshape or retire the code, as later honor-court reforms did, but dismantling it means surrendering an informal discipline instrument no regulation replaces cheaply.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, regimental_officer_establishment, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, regimental_officer_establishment, beneficiary).

% Old families whose standing rests on honor capital. The code converts pedigree into deference and screens arrivistes who have not inherited the willingness to stake life on reputation. Their family identity is fused with the honor order; abandoning it would dissolve the distinction they live by, so they defend the code even as its costs mount.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, established_gentry_lineages, beneficiary,
    powerful, generational, identity_locked, national).

% Fencing masters, pistol smiths, professional seconds, and retained surgeons who sell services into the affair economy. Collect fees per encounter and per lesson; least invested in the code's ideology and quickest to pivot to sport instruction and civilian clientele as demand shifts.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_professionals, beneficiary,
    moderate, immediate, mobile, regional).

% Young officers bound to answer challenges. Refusal ends careers and social existence inside the corps; acceptance risks death, maiming, and later criminal prosecution as statutes tighten. Honor is constitutive of their professional self, so exiting the obligation means exiting the identity — resignation, emigration, or a lifetime outside the only career they trained for.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, junior_officers_compelled_to_fight, payer,
    moderate, biographical, identity_locked, national).

% The killed, maimed, widowed, and orphaned. Bear the arrangement's realized harms with no seat in any proceeding that produced them; the code frames their losses as honorable, which forecloses grievance. Occurrence is scattered across regiments and decades, so coalition formation among them is rare and short-lived.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, duel_casualties_and_bereaved, payer,
    powerless, biographical, trapped, local).

% Men who declined the satisfaction demand and absorbed ostracism, mess blackballing, and career termination. Their refusals function as the enforcement displays that teach everyone else compliance; once the refusal is public, the sanction lands regardless of subsequent conduct, and appeals run only to the same honor community that imposed it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, refusing_gentlemen, payer,
    moderate, biographical, trapped, national).

% Legislatures, courts, and military tribunals that prohibit and prosecute dueling across the interval. Spends on enforcement for generations and prosecutes survivors and seconds; consolidates the violence monopoly as the code recedes. Its prohibition feeds back into the honor economy rather than acting on it neutrally: criminal liability raises the price of compliance for men still bound, accelerating the credibility collapse of the honor frame.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Clerical bodies, utilitarian campaigners, and anti-dueling societies. Object publicly, publish statistics of corpses, and lobby for statute, but hold no seat in the honor courts or regimental tribunals where affairs of honor are actually adjudicated. Dependent on persuasion and legislation; their exclusion from the code's internal fora is what the adjudication structure maintains.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, anti_dueling_reformers, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, established_gentry_lineages).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates lethal disputes among armed gentlemen who regard courts as dishonorable: channels conflict into ritualized, rule-governed single combat with seconds, agreed conditions, and recognized reconciliation endpoints (satisfaction, exchanged apologies), preventing feud spirals and preserving corps cohesion without written discipline.
% TRANSFER_FUNCTION: Moves mortal risk, wealth (arms, instruction, seconds' service, travel), and dispute-resolution authority from compelled participants — disproportionately junior officers and unprotected newcomers — to the honor establishment and its professionals; converts compliance into standing and routes conflict resolution away from state courts into the honor community.
% ABSENT_VOICES: The men compelled to fight who privately wanted out — declaring reluctance was structurally unspeakable, since admitting it confessed cowardice. Bereaved families had no standing in any proceeding that produced their loss. Clerical and utilitarian objectors campaigned loudly outside the fora where affairs were adjudicated and were never seated inside them.
% DISAPPEARANCE_RATIONALE: If the satisfaction obligation and its enforcement machinery vanished overnight, officer promotion and reputation economies would reroute through written evaluation and state discipline, dispute resolution among gentlemen would shift to the courts, the affair economy of seconds and fencing masters would collapse into sport, and the class boundary the code policed would need a new marker — the arrangements built on it reorganize rather than persist.
% FOUNDING_PROBLEM: Bounding feud escalation among private armed elites: pre-modern nobilities and officer corps needed a way to manage lethal quarrels without vendetta cycles destroying lineages and units, and the duel ritualized and bounded the violence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nineteenth-century utilitarian critics and clerical writers argued the original feud-management function was obsolete under state violence monopolies; military professionals themselves conceded that courts-martial and written discipline had absorbed the function; and modern historiography of the duel (Kiernan, Frevert, Nye) documents the founding problem dissolving generations before the practice did. No attestation of a live founding problem exists outside the honor community's own ranks.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base properties are authored at interval end and matched to the final measurement points per the end-state alignment convention: extractiveness 0.38, suppression 0.41, theater_ratio 0.58. The temporal series carry the analytical weight, all on one shared grid (t = 0,10,...,80). Base extractiveness peaks at 0.71 around t=30 rather than declining monotonically: statutory prohibition stacked criminal liability on top of mortal risk for men the code still bound, while refusal sanctions held — the enforcement ratchet through which the exogenous strand temporarily intensified the arrangement's hold on its targets. Suppression_requirement follows the same arc peaking later (0.76 at t=40): the code's internal machinery worked hardest precisely as its legitimacy wobbled, because defection became attractive exactly when external law made compliance costly; after the midpoint the machinery attrites and the series falls. Theater_ratio rises monotonically to 0.58 as surviving observances degrade into performance — first-blood encounters, scoring-oriented student duels — functional ritual becoming display. Accessibility_collapse 0.70: inside the honor frame, alternatives (apology without stigma, recourse to courts) collapsed almost completely, but the frame never attained natural-law totality — legal exits existed and widened across the interval. Resistance 0.45: clerical campaigns, utilitarian critique, anti-dueling societies, and celebrated refuser cases met real but long-ineffective opposition; resistance succeeded only as the endogenous strand matured. Coalition note: the payer seats (compelled juniors, refusers) shared a structural position and occasionally coordinated — evangelical officer networks, published refuser defenses — but identity lock fragmented durable coalition, since joining a refusal movement was itself honor-costly. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute differently, and the state seat differently again. From inside the regimental establishment and the gentry lineages, the arrangement is constitutive order: it converts pedigree into deference, disciplines juniors without written rules, and bounds violence among armed men who scorn courts. From the compelled junior's position the identical structure is mortal coercion with an identity-locked exit — honor is his professional self, so refusing the obligation means ceasing to be what he is. The bereaved and the refusers hold the realized-cost positions: harm landed, or sanction displayed, with no procedural seat. The state apparatus experiences the arrangement as a sovereignty defect — a rival dispute-resolution and violence-routing authority — and its prohibition policy feeds back into the honor economy rather than acting on it neutrally. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (established_gentry_lineages, regimental_officer_establishment, dueling_professionals) derive low directionality — the arrangement subsidizes them with status rents, command authority, and fee income. Victim declarations (junior_officers_compelled_to_fight, duel_casualties_and_bereaved, refusing_gentlemen) derive high directionality, amplified toward the full-target end by identity_locked and trapped exit positions. The state_legal_apparatus sits near symmetric: it pays enforcement costs and collects violence-monopoly consolidation but is not a transfer participant in the honor economy. No directionality override is authored: the derivation distinguishes the two institutional seats by role and exit rather than by power atom alone, and an institutional-level override would misfire across both. Receipt surface: gains demonstrably accrue to established_gentry_lineages, whose honor capital the arrangement maintains and converts into deference; the establishment collects a real but secondary deference dividend, and dueling_professionals collect fees at the margin. Fixing cost is prohibitive: dismantling the code required a century of legislation, prosecution, and honor-court reform, and for the establishment it meant dissolving the status economy it lived by — costs high relative to the benefit of ending compelled killing, which is why the arrangement outlived its founding problem by generations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding feud escalation among private armed elites — died when states consolidated the violence monopoly and militaries professionalized, yet the arrangement persisted for generations as status ritual: founding_problem_status 'dead' combined with disappearance_verdict 'world_rearranges' is the capture/zombie signature, and the rising theater_ratio corroborates it mechanically. The composite classification is what keeps this visible. Read as pure rope-breaking (the exogenous-only sibling), the ratchet phase disappears — the interval where legal pressure made the arrangement MORE costly for its targets while still binding them — and the ceremonial remnant looks like mere lag. Read as pure cultural dissolution (the endogenous-only sibling), the enforcement machinery's intensify-then-attrite arc disappears, and prohibition looks decorative. The tangled_rope claim over the operative life, with metrics drifting toward a theatrical remnant, models both strands and their entanglement: coordination function real, extraction real, decay driven by their interaction. Mandatrophy is resolved in fact — the mandate outlived its function — and the mismatch flag fires on honest authorship rather than being tuned away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the composite_overdetermined_reading of kernel honor_satisfaction_substrate — what structural classifications would the sibling readings (practice_decline_reading, cultural_contraction_reading) assign to the same arrangement, and where exactly does the disagreement bite?',
    'Compile and classify all three reading-stories of the kernel; compare per-seat types and epsilon attributions across the family.',
    'If the siblings compute materially different types over the same referent, the kernel''s contest is located in epsilon attribution and victim-set composition rather than in the underlying record; the composite''s distinctness rests on the non-independence axiom, which no sibling encodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-of-kernel status and sibling structural deltas.').

omega_variable(
    pathway_nonindependence_identifiability,
    'Is the claimed non-independence of the suppression and delegitimation pathways empirically distinguishable from mere simultaneity of two additive causes?',
    'Cross-jurisdictional comparative timing: relate decline rates to prohibition intensity and honor-economy exposure; a positive interaction term (accelerated decline where both are high) evidences non-independent pathways, while additive effects with no interaction support the siblings'' separable-causes picture.',
    'A null interaction collapses the composite into the two sibling readings held side by side and dissolves its claim to a distinct constraint; a robust interaction secures the composite as the structurally correct reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathway_nonindependence_identifiability, empirical, 'Identifiability of causal entanglement versus additive co-occurrence.').

omega_variable(
    reflexive_synthesis_confirmation_risk,
    'The composite reading was synthesized from the same historiographic record that tests it — how much of its fit is construction rather than discovery?',
    'Preregistered coding of primary sources (challenge frequencies, refusal sanctions, prosecution records) against predictions unique to the composite — e.g., refusal-sanction intensity should peak mid-transition rather than move monotonically.',
    'Failure of composite-unique predictions demotes the reading to an unfalsifiable synthesis and revives the siblings as live competitors; success converts the drift_state stable verdict from reflexive to earned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reflexive_synthesis_confirmation_risk, empirical, 'Confirmation risk in a post-hoc synthetic reading.').

omega_variable(
    identity_lever_internalization_share,
    'How much of the force binding compelled participants was internalized (honor as constitutive self) versus structurally enforced (career and ostracism machinery)?',
    'Refusal-trajectory analysis: compare later-life outcomes and stated motives of refusers across regiments with differing enforcement intensity; persistence of compliance-driven behavior after removal of enforcement machinery indicates internalization.',
    'A high internalized share raises effective suppression for identity_locked seats beyond the structural measure and sharpens the identity_coordination gaming caution — the identity lever was simultaneously the coordination surface and the extraction instrument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lever_internalization_share, empirical, 'Structural versus internalized share of the code''s coercive force.').

omega_variable(
    remnant_phase_boundary,
    'Does the interval-end ceremonial remnant (first-blood encounters, scarring-oriented student duels) belong to this constraint''s degraded drift phase or constitute a successor arrangement with its own epsilon?',
    'Track whether remnant practitioners inherit the satisfaction obligation (same kernel, degraded function) or pursue a new practice whose justification is the practice itself; discontinuity in justification marks a successor.',
    'Same-kernel continuation validates reading the rising theater_ratio as lifecycle drift of this constraint; successor-status splits the story and moves the terminal classification to a new file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remnant_phase_boundary, conceptual, 'Whether the ceremonial remnant is this constraint''s inertial phase or a successor constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_composite_reading_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t0, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t10, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t10, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t20, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t20, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t30, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t30, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t40, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t40, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t50, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t50, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t60, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t70, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 70, 0.53).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t70, observed).
narrative_ontology:measurement(hss_composite_reading_tr_t80, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement_basis(hss_composite_reading_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(hss_composite_reading_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(hss_composite_reading_be_t0, observed).
narrative_ontology:measurement(hss_composite_reading_be_t10, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(hss_composite_reading_be_t10, observed).
narrative_ontology:measurement(hss_composite_reading_be_t20, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement_basis(hss_composite_reading_be_t20, observed).
narrative_ontology:measurement(hss_composite_reading_be_t30, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(hss_composite_reading_be_t30, observed).
narrative_ontology:measurement(hss_composite_reading_be_t40, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hss_composite_reading_be_t40, observed).
narrative_ontology:measurement(hss_composite_reading_be_t50, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(hss_composite_reading_be_t50, observed).
narrative_ontology:measurement(hss_composite_reading_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(hss_composite_reading_be_t60, observed).
narrative_ontology:measurement(hss_composite_reading_be_t70, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 70, 0.44).
narrative_ontology:measurement_basis(hss_composite_reading_be_t70, observed).
narrative_ontology:measurement(hss_composite_reading_be_t80, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(hss_composite_reading_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(hss_composite_reading_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hss_composite_reading_su_t0, observed).
narrative_ontology:measurement(hss_composite_reading_su_t10, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(hss_composite_reading_su_t10, observed).
narrative_ontology:measurement(hss_composite_reading_su_t20, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(hss_composite_reading_su_t20, observed).
narrative_ontology:measurement(hss_composite_reading_su_t30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(hss_composite_reading_su_t30, observed).
narrative_ontology:measurement(hss_composite_reading_su_t40, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(hss_composite_reading_su_t40, observed).
narrative_ontology:measurement(hss_composite_reading_su_t50, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(hss_composite_reading_su_t50, observed).
narrative_ontology:measurement(hss_composite_reading_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(hss_composite_reading_su_t60, observed).
narrative_ontology:measurement(hss_composite_reading_su_t70, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 70, 0.5).
narrative_ontology:measurement_basis(hss_composite_reading_su_t70, observed).
narrative_ontology:measurement(hss_composite_reading_su_t80, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement_basis(hss_composite_reading_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did dueling die?' decomposes into three structurally distinct readings of the kernel honor_satisfaction_substrate, each with its own epsilon attribution, victim-set emphasis, and classification. This composite reading links to both siblings because it structurally influences them — it demotes each monocausal account to a partial-factor status — while the practice_decline sibling stands in a contradiction relation to it (see cs_structure.reading_relations). Upstream/downstream: the siblings were articulated first; the composite synthesizes their evidence bases and exerts downstream legitimacy pressure on both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
