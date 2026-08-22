% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Pathway: Communion Rebuilt Through Patient Participatory Labor
 *   domain: religious/political_theology/technology_ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the human_transcendence_pathway
 *   kernel: the Jerusalem reading, in which authentic community is rebuilt
 *   through patient, participatory labor under divine blessing, integrating
 *   plurality into communion rather than erasing it into uniformity. The
 *   archetype is the post-exilic rebuilding — dispersed returnees
 *   reconstructing walls, temple, and commons household by household under
 *   teaching and blessing rather than compulsion — carried forward as a
 *   standing pattern in Catholic social doctrine (subsidiarity, solidarity,
 *   communion-over-uniformity) and applied today against
 *   technocratic-acceleration models of community. The constraint
 *   coordinates: it solves a real collective-action problem (rebuilding
 *   without coercive capacity) at low extraction, with the whole community
 *   and especially the marginalized as beneficiaries and no structurally
 *   identified victim group. Assumptions stated: the interval runs in
 *   generational units, t0 anchored to the post-exilic return and t60 to the
 *   contemporary doctrinal application; epsilon's referent is the standing
 *   participatory-rebuilding arrangement AS THIS READING SEES IT — never the
 *   technocratic alternative it critiques. KEY AGENTS (by structural
 *   relationship): - returning_exiles: primary beneficiary
 *   (powerless/constrained) — gain land, security, and standing through
 *   shared labor - resident_households: beneficiary-payer
 *   (moderate/constrained) — bear the sacrificed efficiency, receive the
 *   secured commons - community_elders_and_priests: agenda_setter
 *   (institutional/identity_locked) — administer, teach, and bless;
 *   structurally unable to coerce without dissolving their office -
 *   rising_generation_in_formation: beneficiary-payer (powerless/constrained)
 *   — formed into commitments made before their consent -
 *   neighboring_peoples_of_the_land: excluded (moderate/mobile) — outside the
 *   boundary the pattern draws - social_doctrine_analysts: analytical
 *   observer — sees the full structure from outside its administration
 *
 * KEY AGENTS:
 *   - returning_exiles: primary beneficiary (powerless/constrained) — marginalized returnees whose route to standing runs through the shared labor
 *   - resident_households: beneficiary-payer (moderate/constrained) — bear the opportunity cost of slow participatory methods, receive the commons
 *   - community_elders_and_priests: agenda_setter (institutional/identity_locked) — administer rotations, stores, and formation; their office is fused with the pattern they run
 *   - rising_generation_in_formation: beneficiary-payer (powerless/constrained) — inherit the commons, bear the discipline of formation
 *   - neighboring_peoples_of_the_land: excluded (moderate/mobile) — offered to join, refused, remain outside the assembly
 *   - social_doctrine_analysts: analytical observer (analytical/analytical) — assess the pattern against rival readings from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.22).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.16).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Pathway: Communion Rebuilt Through Patient Participatory Labor").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political_theology/technology_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '75f00c0f-f022-4d18-9774-4d2be6051e7d').
narrative_ontology:cs_kernel_codification('75f00c0f-f022-4d18-9774-4d2be6051e7d', fixed_text).
narrative_ontology:cs_authority_grounding('75f00c0f-f022-4d18-9774-4d2be6051e7d', lineage).
narrative_ontology:cs_interpretation_layer_present('75f00c0f-f022-4d18-9774-4d2be6051e7d').
narrative_ontology:cs_reading_relation('75f00c0f-f022-4d18-9774-4d2be6051e7d', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('75f00c0f-f022-4d18-9774-4d2be6051e7d', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('75f00c0f-f022-4d18-9774-4d2be6051e7d', foundational, communion_constituted_under_divine_blessing).
narrative_ontology:cs_axiom_status(communion_constituted_under_divine_blessing, holdable).
narrative_ontology:cs_axiom_grounding('75f00c0f-f022-4d18-9774-4d2be6051e7d', communion_constituted_under_divine_blessing, theological).
narrative_ontology:cs_axiom('75f00c0f-f022-4d18-9774-4d2be6051e7d', foundational, plurality_constitutive_of_communion).
narrative_ontology:cs_axiom_status(plurality_constitutive_of_communion, holdable).
narrative_ontology:cs_axiom_grounding('75f00c0f-f022-4d18-9774-4d2be6051e7d', plurality_constitutive_of_communion, deontological).
narrative_ontology:cs_reference_frame('75f00c0f-f022-4d18-9774-4d2be6051e7d', patient_participatory_communion_under_blessing).
narrative_ontology:cs_drift_state('75f00c0f-f022-4d18-9774-4d2be6051e7d', contemporary_accelerated_technological_society, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75f00c0f-f022-4d18-9774-4d2be6051e7d', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, resident_households).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, rising_generation_in_formation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, resident_households).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, rising_generation_in_formation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced families resettling among ruins. They clear rubble, carry stone, and rebuild on allotted plots; in exchange they receive land title, defensive security, and a recognized place in the assembly. Leaving again would mean renewed rootlessness, so their commitment is settled even though the work is slow and the payoff distant.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    powerless, generational, constrained, regional).

% Families already holding homes in and around the rebuilding site. Each takes responsibility for a section of the common wall and contributes labor days and produce to the shared stores. They give up faster, cheaper building methods and seasonal income so that no household builds alone; what they receive is a secured commons and neighbors bound to them by shared work.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, resident_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, resident_households, payer).

% The teachers, scribes, and priests who organize labor rotations, teach the formation that binds the community, and administer the shared stores. Their authority rests on continuity with the transmitted law and on visible service; they cannot compel participation by force without dissolving the office they hold, so they persuade, schedule, and bless. Their own lives are spent inside the pattern they administer.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, community_elders_and_priests, agenda_setter,
    institutional, generational, identity_locked, regional).

% Children and young adults being formed into the community's practices. They bear the discipline of formation — memorized law, festival obligations, apprenticeship in the trades of rebuilding — and inherit the completed commons. Their consent is gradual rather than initial; they grow into commitments their elders made before they could object.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, rising_generation_in_formation, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, rising_generation_in_formation, payer).

% Settlements around the rebuilding site who offered to join the work and were declined, and who in turn offered assimilation through intermarriage that the elders also declined. They trade with the community, watch its walls rise, and remain outside the assembly where its rules are made.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, neighboring_peoples_of_the_land, excluded,
    moderate, biographical, mobile, regional).

% Theologians, historians, and social researchers who study the pattern from outside its administration — comparing it with coerced-uniformity and technocratic-acceleration alternatives, tracing who bore its costs across generations, and assessing whether its low-coercion character survives translation into modern institutions.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, social_doctrine_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__jerusalem_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of rebuilding a dispersed, internally diverse community without coercive capacity: aligning household labor on shared structures, distributing stores across lean seasons, and transmitting commitment to a generation that did not choose the project — through labor rotation, formation, shared festival, and a blessing-narrative that makes patience credible.
% TRANSFER_FUNCTION: Moves labor days, produce, and building skill from households to the common structures; moves standing, land allotment, and belonging to the returning exiles who join the work; moves formation and interpretive authority from the elder teachers to the rising generation; and moves prestige toward visible service rather than command.
% ABSENT_VOICES: The neighboring peoples who asked to share the labor and were refused would object that a communion claiming to integrate plurality drew its boundary just short of them. Women, whose labor the record counts but whose voices the assembly rarely records, would ask why formation prepared them to serve the commons while others decided for them. The poorest households, who cannot advance labor-days against future harvests, would ask who waits longest for the walls. None of these seats sits in the council where the pattern is administered.
% DISAPPEARANCE_RATIONALE: If the pattern vanished overnight, the rebuilding would reorganize around the nearest available substitutes: coerced corvee under a strongman, wage labor under whoever held the stores, or abandonment of the site altogether. The returning exiles would lose their route to standing; the commons would fragment into defended plots; the formation that reproduces the community across generations would lapse within one.
% FOUNDING_PROBLEM: Post-exile fragmentation: a displaced people returned to ruins, needing to rebuild dwellings, defenses, worship, and shared identity without a king or coercive apparatus, across deep internal diversity of families, tribes, and classes, under pressure from surrounding settlements.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on post-exilic rebuilding communities and the contemporary community-development and social-capital literature corroborate from outside the benefiting parties that the founding problem — rebuilding social fabric after displacement without coercive capacity — remains live wherever displacement and institutional collapse occur. Sociological studies of civic fragmentation attest the problem independently of the tradition that claims this solution.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.22 at interval end) because the arrangement's costs are real but diffuse: households sacrifice faster building methods and seasonal income, formation disciplines the young before they can consent, and the elder-administered stores create mild administrative overhead — yet no identifiable group pays so that another collects. Suppression is authored low (0.16) as a RAW structural property, unscaled by power or scope: the pattern runs on persuasion, formation, festival, and blessing-narrative, not on barriers to exit; the engine scales only extractiveness. Theater ratio is low (0.20): the labor and the liturgy are the same activity at t0, and the ratio rises only as commemoration and institutional maintenance grow relative to direct rebuilding. Accessibility collapse is moderate-low (0.35): genuine alternatives — corvee under a strongman, wage labor under storeholders, dispersal — remain live options the pattern must out-persuade rather than foreclose. Resistance is moderate-low (0.28): prophetic criticism of elites, grumbling at the pace, and boundary disputes meet the pattern without threatening it. The claimed type (rope) is asserted independently of these metrics; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take. The temporal series runs on ONE shared seven-point grid (both metrics authored at every point 0-60) so no end-state value is substituted into earlier rows; no suppression_requirement series is authored because the enforcement picture is static — the persuasive apparatus does not ratchet. The trajectories rise gently and plateau by t50-t60: institutional consolidation completes, and the residual drift stabilizes rather than accumulating.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the elder seat the arrangement is a vocation: they administer it, are formed by it, and cannot exit without dissolving their own office — near-symmetric position with identity-fused investment. From the returning-exile seat the same structure is the first security in a lifetime — strongly subsidized, deeply committed. From the resident-household seat it is a fair trade: real cost, real return, mildly beneficiary-side. From the excluded-neighbor seat it is a boundary drawn just short of them — outside the computation but shaping it. The analyst seat sees all four at once. The engine computes this divergence from power, exit, and the beneficiary declarations; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Three beneficiary groups are declared, driving their derived directionality toward the beneficiary end: returning_exiles (powerless, constrained exit — the constraint subsidizes them most heavily), resident_households (moderate, constrained — net beneficiaries who also pay in labor), and rising_generation_in_formation (powerless, constrained — subsidized inheritance offset by formation discipline). No victims are declared because none is structural: the efficiency sacrificed is converted to common structures, not transferred to a collector. The elders declare no beneficiary/victim position, so the canonical fallback places them near symmetric — appropriate, since they both run the arrangement and spend their lives inside it; their identity_locked exit keeps them invested rather than extractive. The excluded neighbors hold mobile exit and no declared stake, so their directionality sits mid-range with minimal contribution to effective extraction anywhere. Regional scope keeps scope-amplification modest. No directionality overrides are authored: the derivation from declarations and exit options already produces the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Romantics would claim pure harmony — hiding the real diffuse costs (sacrificed efficiency, pre-consent formation) that the metrics honestly record. Cynics would claim covert coercion — that religious formation is extraction wearing vestments; the structural data answers: no suppressed exits, no concentrated capturer, a live founding problem corroborated from outside the beneficiary set. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag; the theater trajectory (0.07 to 0.20) stays well below the proxy-replacement threshold. The temporal series watches exactly the failure mode that would open the piton path: if the pattern ever persisted after communion ceased to be its aim, theater_ratio would climb past 0.5 while extraction stayed diffuse — performance maintaining an inertial shell. Nothing in the current record shows that turn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (jerusalem_reading) of the human_transcendence_pathway kernel; what would change structurally if a sibling reading (babel_reading or technocratic_vs_incarnational_reading) were adopted instead?',
    'Cross-reading comparison of the compiled sibling stories: babel_reading should compile with materially higher epsilon and identifiable victims (uniformity imposed on the diverse); the transhumanist pole of technocratic_vs_incarnational_reading should show extraction concentrated on those whose limits are optimized away.',
    'Adopting the babel reading would relocate this constraint''s beneficiaries into a victim set and raise epsilon sharply; adopting the incarnational pole would converge toward this reading''s profile, while the transhumanist pole diverges on every structural element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame indexicality: which reading of the transcendence-pathway kernel is instantiated, and what siblings would change.').

omega_variable(
    boundary_inclusion_ambiguity,
    'Does the communion-that-integrates-plurality extend past the covenant boundary — to the neighboring peoples who offered to join and were refused — or does the pattern presuppose a bounded community whose margins quietly absorb exclusion costs?',
    'Trace the tradition''s own boundary cases across the corpus: the inclusive strains (the Ruth and Jonah currents, the guest-at-the-table practice) against the exclusionary edicts of the rebuilding era, and their reception in modern social doctrine''s treatment of outsiders.',
    'If the boundary systematically excludes, undeclared victims exist at the margin, effective extraction rises for the excluded, and the classification drifts toward tangled_rope territory; if the boundary is porous in principle, the rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_inclusion_ambiguity, empirical, 'Whether the pattern''s integration of plurality reaches outsiders or stops at the covenant line.').

omega_variable(
    formation_voluntariness_ambiguity,
    'Is the low measured suppression genuine voluntariness, or internalized conformity that would surface as resistance if the formation apparatus relaxed?',
    'Observe communities where formation intensity drops — lapsed practice, secularized descendants, converts raised outside the pattern: if adherence collapses abruptly rather than decaying gently, the suppression was carried internally, not chosen freely.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the discipline travels with the agent after exit — and the rope reading overstates the voluntariness of consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_voluntariness_ambiguity, empirical, 'Structural versus internalized component of the pattern''s low suppression.').

omega_variable(
    efficiency_cost_distribution,
    'Who bears the opportunity cost of slow participatory methods across lean seasons — is the sacrificed efficiency distributed evenly, or do the poorest households wait longest for walls, stores, and security?',
    'Household-level timing analysis of burden across the rebuilding cycle: correlate labor-day levies and deferred consumption with household reserves, using the historical record and comparable modern participatory-rebuilding projects.',
    'A regressive burden would raise effective extraction specifically for powerless beneficiary seats, complicating the clean beneficiary profile; an even or progressive burden confirms the solidarity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_cost_distribution, empirical, 'Distribution of the efficiency sacrifice across household strata.').

omega_variable(
    institutional_drift_valence,
    'Is the gentle upward drift in extractiveness and theater across the interval benign institutional maturation (ritual thickening, professional administration) or the earliest layer of rent-seeking on a coordination structure?',
    'Watch whether the plateau holds: if theater_ratio resumes climbing past 0.5 while the participatory substance thins, the drift is Goodhart substitution; if it stabilizes with the commons intact, it is maturation.',
    'Rent-seeking confirmation would date a rope-to-tangled_rope transition and identify the scribal-administrative seat as proto-capturer; maturation confirmation leaves the rope reading intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_drift_valence, empirical, 'Valence of the observed institutional drift: maturation versus early extraction accumulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__jerusalem_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(huma_tr_t50, human_transcendence_pathway__jerusalem_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(huma_tr_t60, human_transcendence_pathway__jerusalem_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(huma_be_t50, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(huma_be_t60, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 60, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how humanity pursues transcendence and authentic community' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories sharing one kernel: babel_reading (self-sufficient unified power; high epsilon, identifiable victims in the homogenized), technocratic_vs_incarnational_reading (compound: optimization-versus-grace; divergent epsilon by pole), and this story, jerusalem_reading (patient participatory communion under blessing; low-to-moderate epsilon, no structural victims). Each file carries its own beneficiaries, metrics, and claimed type; the edges here record the family linkage so contamination and legitimacy analysis can propagate across readings. Upstream/downstream: the babel and technocratic readings are frequently cited AGAINST this one as the realistic alternatives; this reading's persistence changes the legitimacy conditions under which they argue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
