% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Sacrifice Commitment Symbolic Transformation (Study and Prayer as Authoritative Substitution)
 *   domain: religious_law/commitment_system
 *
 * SUMMARY:
 *   The rabbinic transformation of sacrifice commitment from material Temple
 *   performance to study and prayer represents either (1) authorized
 *   hermeneutical reinterpretation enabling practice continuity across
 *   diaspora or (2) unauthorized institutional drift disguising extraction of
 *   authority under cover of legitimate redefining. THIS READING instantiates
 *   the first interpretation: that the Rabbis possessed hermeneutical
 *   authority to transform the commitment's fulfillment conditions, and that
 *   prayer and study are the new instantiation, not mere suspensions or
 *   substitutes for a practice awaiting restoration. This reading generates
 *   high extractiveness (0.68 at interval end) because it claims authority to
 *   redefine divine command — a power that, if not legitimately held, becomes
 *   institutional extraction from those who hold the original
 *   command-performance link as binding. The constraint exhibits tangled_rope
 *   structure: genuine coordination function (maintaining practice continuity
 *   for diaspora communities who cannot perform material sacrifice) paired
 *   with asymmetric extraction (transferring hermeneutical authority from
 *   literal text to rabbinic gatekeepers, imposing costs on those who reject
 *   the transformation). Suppression is high (0.72) because the
 *   transformation's persistence depends on actively marginalizing literalist
 *   and restoration readings, not on participant preference for the new
 *   instantiation.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure — institutional agenda-setter; declares and enforces transformation; benefits from hermeneutical consolidation and diaspora continuity
 *   - diaspora_jewish_communities — organized beneficiaries with constrained exit; depend on transformation to practice religion without Temple; bear cost of accepting redefinition
 *   - literalist_halakhists — moderate payers with identity-locked exit; argue transformation lacks authority; experience existential unfulfilled obligation
 *   - temple_restoration_advocates — powerful payers with constrained exit; view transformation as provisional suspension, not authorized redefining; bear cost of navigating competing institutional claims
 *   - jewish_textual_scholarship — excluded observers; examine hermeneutical legitimacy of transformation; external to internal halakhic determinations
 *   - analytical_observer — structural observer; examines whether transformation is authorized or extraction disguised as reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.68).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.72).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Sacrifice Commitment Symbolic Transformation (Study and Prayer as Authoritative Substitution)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'afde2928-e894-482c-bb31-88317478eceb').
narrative_ontology:cs_kernel_codification('afde2928-e894-482c-bb31-88317478eceb', fixed_text).
narrative_ontology:cs_authority_grounding('afde2928-e894-482c-bb31-88317478eceb', extraction).
narrative_ontology:cs_interpretation_layer_present('afde2928-e894-482c-bb31-88317478eceb').
narrative_ontology:cs_reading_relation('afde2928-e894-482c-bb31-88317478eceb', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('afde2928-e894-482c-bb31-88317478eceb', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('afde2928-e894-482c-bb31-88317478eceb', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('afde2928-e894-482c-bb31-88317478eceb', foundational, rabbinic_authority_redefines_divine_command).
narrative_ontology:cs_axiom_status(rabbinic_authority_redefines_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('afde2928-e894-482c-bb31-88317478eceb', rabbinic_authority_redefines_divine_command, deontological).
narrative_ontology:cs_axiom('afde2928-e894-482c-bb31-88317478eceb', foundational, transformation_permanent_not_provisional).
narrative_ontology:cs_axiom_status(transformation_permanent_not_provisional, holdable).
narrative_ontology:cs_axiom_grounding('afde2928-e894-482c-bb31-88317478eceb', transformation_permanent_not_provisional, conventional).
narrative_ontology:cs_reference_frame('afde2928-e894-482c-bb31-88317478eceb', rabbinic_hermeneutical_supremacy).
narrative_ontology:cs_drift_state('afde2928-e894-482c-bb31-88317478eceb', contemporary_pluralist_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('afde2928-e894-482c-bb31-88317478eceb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhists).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, temple_restoration_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declared and enforces the transformation from material Temple sacrifice to study and prayer as fulfillment of divine command. Exercises hermeneutical authority to certify what counts as legitimate performance of the commitment. Controls the interpretive apparatus and institutional mechanisms that adjudicate whether an individual or community is in good standing with the obligation. Benefits directly from the consolidation of this authority and from the continuity it enables for diaspora communities. Can exit this position only by surrendering hermeneutical authority itself (arbitrage: the authority can abandon this specific transformation and adopt another, but abandoning the role as authority-setter is structurally possible but would require surrendering institutional legitimacy).
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefit from the transformation because it permits them to practice Jewish religion continuously in the diaspora, where material Temple sacrifice is impossible. Prayer and study are accessible everywhere; the transformation solves a genuine coordination problem (how to maintain religious commitment and identity without access to Temple). They also bear costs by accepting that the commitment has been redefined — a redefinition that some understand as legitimate adaptation and others as unauthorized drift. Exit would mean either rejecting the transformation and accepting permanent unfulfilled obligation (identity-locked barrier) or leaving the Jewish community (social/identity cost). Directed to pay 0.38 (symmetric to beneficiary, slightly payer-leaning, due to the secondary role and the real cost of accepting redefinition) — overridden from derived ~0.35 to account for the fact that the coordination benefit is substantial and genuine, not captured by default symmetric calculation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities, payer).

% Argue that the divine command to offer sacrifice is binding and non-negotiable, and that study is at best a temporary substitute until material conditions permit restoration. They believe they do not fully occupy the divine commitment through study alone — the obligation remains unfulfilled. They bear the cost of living under institutional frameworks that certify study as fulfillment while they hold it as insufficient. Their exit from this position would require denying their reading of the command's binding nature and accepting the rabbinic transformation as legitimate, which is fused with their religious identity and interpretation of Torah — identity-locked barrier. Even if they leave traditional institutional structures, they carry the sense that the commitment remains unfulfilled.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhists, payer,
    moderate, biographical, identity_locked, regional).

% Hold that the transformation is valid only provisionally — the commitment is suspended, not transformed, pending Temple restoration. They live in a religious environment where the institutional apparatus claims their core obligation (Temple sacrifice) has been authoritatively reframed as prayer and study. They bear the cost of navigating a world where the authority structure asserts the transformation is permanent and binding, while they hold it as temporary. Their exit would require either accepting the transformation as permanent (denying the possibility or necessity of Temple restoration) or leaving the communities that enforce the transformation. Constrained exit: they cannot easily leave Jewish tradition without cost, but staying requires submitting to institutional framing they contest.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, temple_restoration_advocates, payer,
    powerful, civilizational, constrained, national).

% Academic and literary scholars (historians, philologists, textual critics, religious studies academics) who examine whether the Rabbis possessed legitimate hermeneutical authority to redefine divine command. They analyze sources, reconstructed historical contexts, and the logic of the transformation claim. They are excluded from internal halakhic determination of what counts as legitimate reading in Jewish law — their scholarship feeds external understanding and provides alternative frameworks for evaluation, but it does not bind Jewish communities that prioritize rabbinic authority. Their exclusion is structural: they speak from outside the tradition's internal authority system. Their scholarship shapes visibility of alternatives and raises costs for institutional suppression of dissenting readings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, jewish_textual_scholarship, excluded,
    organized, generational, constrained, global).

% Examines the constraint's structure from outside any committed position: does the transformation represent authorized hermeneutical reinterpretation enabling diaspora practice continuity, or unauthorized institutional extraction of commitment meaning? Takes testimony from all seated positions, examines whether the authority claim is legitimate or coercive, tracks how suppression and resistance evolve over time, and assesses whether the constraint enables genuine coordination or enforces asymmetric institutional benefit. Neither collects nor pays; purely structural observer.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of Jewish religious commitment and community identity after Temple destruction. Without the transformation, the diaspora Jewish community would face either: (1) suspension of the sacrifice commitment indefinitely (generating permanent unfulfilled obligation and spiritual coherence crisis), (2) schism between those who accept diaspora constraints and those who insist on continued Temple orientation, or (3) dissolution of the tradition as binding practice. The transformation coordinates diaspora observance around a unified practice framework (prayer and study) that all positions can engage with, even while disagreeing about what it means.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from literal textual command (divine instruction to sacrifice) to rabbinic institutional interpreters (those who certify what counts as fulfillment and legitimate observance). In material terms, this moves interpretive power from the text's prima facie meaning to the institutional apparatus. Also transfers religious authority and legitimacy: whether individuals experience their observance as occupying the divine commitment or merely preserving a defunct practice depends on whether they accept the rabbinic transformation as authoritative.
% ABSENT_VOICES: Temple priests and their descendants (whose authority and function were grounded in material sacrifice performance) are rendered structurally obsolete by the transformation and have no voice in diaspora practice determination. Ancient communities who held the command-performance link as inviolable and divinely mandated are not present to contest the reframing. Academic textual scholars questioning hermeneutical legitimacy are excluded from internal halakhic verdicts; their scholarship provides external perspective but is not binding on communities that prioritize rabbinic institutional authority.
% DISAPPEARANCE_RATIONALE: If the rabbinic transformation were overturned and literalist performance demand reinstated, diaspora Jewish communities would restructure fundamentally: either schism between Temple-oriented and diaspora-adapted factions, or dissolution of unified practice. Alternatively, communities would reject the commitment as non-binding, ending the practice continuity that transformation preserved. If restoration advocates succeeded in overturning transformation in favor of suspension-pending-restoration, institutional framework would shift from certification of prayer/study as fulfillment to certification of prayer/study as preparatory placeholder — substantively different religious consciousness and community organization.
% FOUNDING_PROBLEM: After Temple destruction in 70 CE, the Torah's commandment to offer sacrifice could not be performed. The Jewish community faced a coordination crisis: how to maintain a binding divine commitment without access to the material and institutional conditions (Temple building, priestly service, ritual purity system) required for its performance. Three possible responses existed: (1) declare the commitment suspended indefinitely (abandoning active practice), (2) accept permanent unfulfilled obligation (spiritual incoherence), or (3) redefine the commitment's fulfillment conditions to permit diaspora observance. Rabbinic authorities chose the third option.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authority structure attests the problem was real and the transformation was necessary and authorized. Mishnaic and Talmudic texts document extensive reasoning about sacrifice and its alternative (prayer and study) as fulfillment, suggesting the transformation was deliberate and institutionally endorsed. Medieval and modern Jewish codes universally accept the transformation as binding law (corroboration from institutional continuity). However, literalist and restoration-focused readings (held by Karaite communities historically and segments of Haredi and Temple-focused movements today) attest that the founding problem was incorrectly solved — that the Rabbis lacked the authority to redefine divine command, or that the transformation is valid only provisionally. Academic textual scholarship (from outside the tradition's institutional authority) attests that the transformation occurred and was treated as legitimate reframing by rabbinic consensus, but also documents that it was contested and remains philosophically disputable. No corroboration exists from parties neutral to the tradition's own internal commitments.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the high structural cost of the claim to hermeneutical authority. If the Rabbis possessed legitimate authority to redefine divine command, the extraction is the institutional consolidation of that authority (institutional beneficiary, constrained payers). If the claim to authority is itself unauthorized drift, the extraction is wholesale institutional capture of commitment meaning, with victims being those who hold original performance as non-negotiable. The measurement series reflects this: extractiveness rises from 0.31 (immediate post-Temple, alternative readings plausible) to 0.68 (modern, institutional apparatus fully consolidated but scholarly challenges rising). Suppression follows the same trajectory, peaking before modernity (0.69) when traditional authority was unchallenged, then increasing again (0.72) as the apparatus must now work harder to maintain consensus against scholarly and pluralist objections. Theater ratio climbs from 0.08 to 0.41, reflecting the growing disconnect between authentic performance (when Temple restoration seemed plausible) and archival/preserved practice (when it became clear the transformation was permanent). All measurements share a single time grid, with observations anchored at historical periods: T=0 (immediately post-70 CE), T=200 (Mishnaic consolidation), T=500 (post-Talmudic codification), T=1000 (medieval institutional stability), T=1750 (pre-modernity peak), T=1960 (modern pluralism rising).
 *
 * PERSPECTIVAL GAP:
 *   Rabbinic authority and diaspora communities compute this constraint very differently. From the institutional seat, the transformation is genuine coordination enabling diaspora practice continuity — a legitimate exercise of hermeneutical authority to adapt divine command to historical circumstance. From the literalist seat, the same structure operates as extraction: institutional redefinition of divine command, with the cost imposed on those who refuse to accept the reframing. From the restoration-advocate seat, it is provisional suspension misrepresented as transformation — institutional extraction of the choice to defer commitment pending Temple restoration. The engine computes per-seat type from the structural data (beneficiary/victim declarations, exit options, power atoms); these divergent computations are exactly what seat-differentiated classification is built to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority structure: beneficiary (controls hermeneutical apparatus, consolidates authority, benefits from continuity it enables), d near 0.0. Diaspora communities: beneficiary (practice continuity, accessible observance) with secondary payer role (accept redefinition as the cost of continuity), d near 0.35–0.40 (symmetric, with slight beneficiary lean because genuine coordination need is met). Literalist halakhists: payer (reject transformation, experience unfulfilled obligation, bear social/institutional pressure), d near 0.85 (target, identity-locked exit makes them structurally trapped). Temple restoration advocates: payer (forced to navigate institutional apparatus claiming their commitment is discharged when they hold it as suspended), d near 0.78 (target, constrained exit because leaving the tradition requires rejecting core beliefs). Excluded scholarship: neither collecting nor paying directly, analytical seat, d at 0.5 (symmetric observer). The directionality derivation flows from beneficiary/victim declarations: beneficiaries get low d, victims get high d, and exit options modulate (identity_locked amplifies target d, arbitrage dampens it). No directionality overrides are needed; the structural derivation captures the seated differences.
 *
 * MANDATROPHY ANALYSIS:
 *   The transformation was established as live commitment during the Talmudic period (roughly 200–500 CE). The founding problem (Temple destruction, impossibility of material performance) remains contested: restoration advocates hold it as unsolved (Temple still needed for full commitment), literalists hold it as persistently alive (obligation unfulfilled), and mainstream diaspora read it as solved (transformation permits genuine practice). The constraint shows no mandatrophy (founding problem has not become obsolete) — the divergence persists because the three readings remain live positions within Jewish tradition. However, the constraint exhibits zombie-drift symptoms: theater ratio rises throughout the interval, and suppression requirement increases in the modern period exactly when one might expect it to decline (if the transformation were fully accepted, suppression would relax). Modern pluralism and scholarly challenge create new resistance, forcing the institutional apparatus to work harder to maintain the transformation's standing. This is piton-adjacent behavior: the function (enabling diaspora practice) remains real, but the distribution of costs and benefits has shifted as the natural consensus that once supported transformation has eroded. The transformation persists not because all seats accept it as authorized, but because institutional authority is sufficient to enforce acceptance among those who do not fully believe it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_authority_legitimacy,
    'Did the Rabbis possess legitimate hermeneutical authority to redefine divine command, or does the transformation represent unauthorized institutional drift disguised as reinterpretation?',
    'Historical-textual examination: (1) Do Mishnaic and Talmudic sources frame the transformation as authorized redefining of commitment versus temporary suspension? (2) Do post-Talmudic dissenting voices (Karaites, literalists) attest that the transformation was contested as lacking authority? (3) Do medieval and modern pluralist traditions recognize the transformation as legitimate reinterpretation or as institutional capture?',
    'If unauthorized drift, the entire constraint recomputes as snare: institutional extraction masked by hermeneutical authority claim, with high extraction borne by those who reject the authority claim. If authorized, the constraint remains tangled_rope: genuine coordination (diaspora practice continuity) paired with asymmetric institutional benefits (hermeneutical consolidation). The classification difference is substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_authority_legitimacy, conceptual, 'Whether transformation is authorized reinterpretation or unauthorized institutional capture of commitment meaning.').

omega_variable(
    transformation_vs_suspension_distinction,
    'Is the status of study and prayer best understood as a transformation of the commitment itself, or as a suspension of the commitment pending Temple restoration?',
    'Textual and phenomenological analysis: (1) Do rabbinic sources treat study-as-fulfillment as permanently authoritative or as temporary placeholder? (2) Do liturgical and legal practices embed expectation of future Temple restoration or treat transformation as permanent redefining? (3) What do contemporary communities report about their own experience: do they feel the commitment is fulfilled, suspended, or occupied through study?',
    'If transformation (this reading''s frame), extractiveness reflects institutional authority consolidation (~0.68). If suspension, extractiveness reflects institutional extraction of the choice to defer commitment, and victims expand to include those who deny the Rabbis could unilaterally suspend divine command. Type may shift from tangled_rope to snare if suspension framing reveals the transformation as coercive redefinition imposed without consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_vs_suspension_distinction, conceptual, 'Whether commitment is transformed to new instantiation or suspended pending restoration.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (institutional barriers, social pressure, exclusion from authority structures) or internalized (those who reject transformation have absorbed the framing that they are heretical/illegitimate)?',
    'Post-exit analysis: if individuals who leave traditional community structures report that suppression persists (they internalized the sense of unfulfilled obligation, illegitimacy of their reading), suppression is partially internalized. If suppression drops post-exit (they felt institutional pressure but hold their reading as legitimate once outside the structure), suppression is primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests — the constraint carries its suppression with people even after institutional exit. This would support snare classification (internalized coercion) over tangled_rope (active enforcement that could be modulated). If structural, suppression reflects active institutional enforcement that could be relaxed, supporting tangled_rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural institutional pressure or internalized by those who reject transformation.').

omega_variable(
    diaspora_exit_analysis,
    'How constrained is the exit for diaspora Jewish communities? Could they accept a literalist reading and maintain their communities, or does the diaspora circumstance genuinely require the transformation?',
    'Counterfactual analysis: (1) Historical examination of small diaspora communities that rejected transformation (Karaites, some literalist Haredi groups) — did they maintain religious coherence? (2) Logical analysis: can material sacrifice be performed outside Temple in any form, or does literalist reading require Temple restoration?',
    'If exit is genuinely constrained by material impossibility of literalist performance, diaspora communities are correctly classified as beneficiaries (they gain practice continuity they couldn''t otherwise have). If exit is constrained only by institutional suppression and social cost, they are better classified as coerced payers (they could reject transformation but face institutional exclusion). Classification shifts their directionality and affects whether they compute as benefiting or suffering from the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_exit_analysis, empirical, 'Whether diaspora exit from transformation is blocked by material constraint or institutional suppression.').

omega_variable(
    modernist_scholarly_challenge_effect,
    'Does the rising scholarly challenge to rabbinic hermeneutical authority (historical-critical study, academic textual analysis) represent a genuine alteration of the constraint''s operative force, or is it a pressure that institutional authority can continue to contain?',
    'Empirical tracking: (1) Measure the proportion of Jewish communities accepting scholarly challenges to transformation authority. (2) Track whether institutional apparatus (denominational authorities, educational curricula) integrates or excludes scholarly critique. (3) Examine whether resistance (measured as 0.54 at interval end) increases in step with scholarly visibility.',
    'If scholarly challenge erodes institutional authority, extractiveness should decrease over time as the authority claim becomes less binding. Rising suppression requirement (0.72) paired with rising resistance (0.54 at interval end, projected to rise further) suggests the constraint is shifting toward snare (requiring more coercion to maintain as authority erodes). If institutional authority successfully contains scholarly challenge, the constraint stabilizes as tangled_rope with rising theater ratio (maintenance becomes more performative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_scholarly_challenge_effect, empirical, 'Whether scholarly challenges to transformation authority alter the constraint''s operative force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 200, 0.15).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.28).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.37).
narrative_ontology:measurement(temp_tr_t1750, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(temp_tr_t1960, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1960, 0.41).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 200, 0.44).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(temp_be_t1750, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1750, 0.65).
narrative_ontology:measurement(temp_be_t1960, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1960, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.54).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.62).
narrative_ontology:measurement(temp_su_t1750, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1750, 0.69).
narrative_ontology:measurement(temp_su_t1960, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1960, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.14).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_commitment kernel decomposes into four structurally distinct constraints, each instantiating a different reading of what the commitment entails after Temple destruction. symbolic_transformation (this file) claims the commitment has been redefining as prayer and study; study_as_exercise claims study itself is the performance of divine command; performance_only claims only material sacrifice satisfies the commitment; hybrid_preparatory claims study maintains the commitment in suspended state pending restoration. These are not the same constraint viewed from different angles — they have different ε values, different victim sets, different institutional stakes. Each reading generates its own constraint story with its own classification. The four stories are linked via affects_constraints to enable analysis of how the kernel contest shapes the constraint family's properties. symbolic_transformation affects the others by claiming the redefining authority that other readings contest or accept provisionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
