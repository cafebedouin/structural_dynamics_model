% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Reading of the Second Commandment — Dulia Through Sanctified Matter
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the iconodule reading of the decalogue image
 *   prohibition kernel: the Second Commandment forbids latria (worship due to
 *   God alone) directed at images, but permits dulia (relative honor) offered
 *   through an image to its prototype, on the theological warrant that the
 *   Incarnation sanctified matter as a legitimate conduit to the divine. This
 *   reading was formalized at the Second Council of Nicaea (787) against the
 *   iconoclast position that held all religious imagery idolatrous, and is
 *   structurally distinct from the moderate-iconoclast reading that permits
 *   two-dimensional images while forbidding statuary. Each reading is
 *   authored as its own constraint with its own epsilon; this one describes a
 *   coordination-dominant structure — sanctioned visual devotion with a real
 *   function and no identified victim class internal to its own operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.22).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Reading of the Second Commandment — Dulia Through Sanctified Matter").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '0a10d855-662b-4e62-9b21-820a0c65b236').
narrative_ontology:cs_kernel_codification('0a10d855-662b-4e62-9b21-820a0c65b236', formalized).
narrative_ontology:cs_authority_grounding('0a10d855-662b-4e62-9b21-820a0c65b236', lineage).
narrative_ontology:cs_interpretation_layer_present('0a10d855-662b-4e62-9b21-820a0c65b236').
narrative_ontology:cs_reading_relation('0a10d855-662b-4e62-9b21-820a0c65b236', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a10d855-662b-4e62-9b21-820a0c65b236', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('0a10d855-662b-4e62-9b21-820a0c65b236', foundational, incarnation_sanctifies_matter_as_conduit).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter_as_conduit, holdable).
narrative_ontology:cs_axiom_grounding('0a10d855-662b-4e62-9b21-820a0c65b236', incarnation_sanctifies_matter_as_conduit, theological).
narrative_ontology:cs_axiom('0a10d855-662b-4e62-9b21-820a0c65b236', foundational, latria_dulia_distinction_is_theologically_stable).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_is_theologically_stable, holdable).
narrative_ontology:cs_axiom_grounding('0a10d855-662b-4e62-9b21-820a0c65b236', latria_dulia_distinction_is_theologically_stable, conventional).
narrative_ontology:cs_reference_frame('0a10d855-662b-4e62-9b21-820a0c65b236', incarnational_material_sanctification).
narrative_ontology:cs_drift_state('0a10d855-662b-4e62-9b21-820a0c65b236', post_reformation_iconoclasm_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0a10d855-662b-4e62-9b21-820a0c65b236', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, hypostatic_union_sanctifies_matter).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, latria_dulia_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Venerate icons in liturgy and private devotion as a tangible means of approaching the divine. The distinction between latria (worship due to God alone) and dulia (honor passed through the image to its prototype) lets them use visual and material practice without formal accusation of idolatry. Exit means abandoning a devotional practice they experience as spiritually load-bearing, not a cost imposed from outside.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, orthodox_laity, beneficiary,
    powerless, biographical, constrained, regional).

% Produce sanctioned images within codified stylistic and doctrinal conventions (canon of orthodox depiction). Their craft is licensed by the reading's permission structure; their livelihood and vocation depend on the interpretive framework holding. They also help set and transmit the boundary of acceptable depiction through guild and monastic training.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, icon_painters, agenda_setter).

% Maintain, copy, and theologically defend icon traditions; some communities (e.g., Stoudios-type monastic centers) historically bore the cost of resisting iconoclast enforcement when the sibling reading held state power. Under this reading they operate with sanctioned continuity rather than persecution.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, monastic_communities, agenda_setter).

% Councils (notably Nicaea II, 787) formalize and enforce the latria/dulia distinction as orthodox doctrine, articulating the Incarnation as theological warrant for material mediation. The hierarchy administers the boundary of permissible depiction and can revise or re-litigate it through further conciliar action; it also derives legitimacy and unifying authority from having settled the question.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Under the sibling reading, imperial and ecclesiastical iconoclast authorities held that any veneration of images was idolatry regardless of intent. Within THIS reading's framework they are structurally excluded — their position is treated as a theological error corrected by conciliar definition, not as a live alternative inside this constraint.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_authorities, excluded,
    institutional, generational, trapped, continental).

% Study the doctrinal, political, and art-historical dimensions of the iconoclast controversies without a stake in either reading's victory; they document how the distinction was formalized and contested across centuries.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, diffuse).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides laity, clergy, and artisans a shared, theologically warranted boundary for using visual and material representation in devotion — solving the coordination problem of how sensory/material practice can approach a transcendent, non-material God without collapsing into idolatry.
% TRANSFER_FUNCTION: Moves interpretive authority and devotional legitimacy toward the ecclesiastical hierarchy that articulates and polices the latria/dulia boundary, and toward icon painters and monastic communities whose craft and vocation the boundary licenses; no material extraction from a victim class is required for the reading to function.
% ABSENT_VOICES: Iconoclast theologians and the authorities who historically enforced image-destruction are excluded from this reading's operative framework — under this reading their position is treated as resolved error, not a live competing claim, even though it remains a live position in the sibling reading and in some traditions today.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the iconoclast or moderate-iconoclast reading became dominant, a substantial devotional and artistic tradition (icon veneration, iconostasis practice, much of Byzantine and Orthodox visual culture) would lose its doctrinal warrant and likely be suppressed or destroyed, as occurred historically during the 8th–9th century iconoclast controversies. Whether the underlying spiritual practice is itself indispensable or merely one contingent cultural expression of piety is exactly what the sibling readings dispute.
% FOUNDING_PROBLEM: How to reconcile the Decalogue's prohibition on graven images with an incarnational theology in which God took visible, material form — and how to permit devotional use of images without licensing idolatry.
% FOUNDING_PROBLEM_CORROBORATION: The Second Council of Nicaea (787) and subsequent Orthodox and Catholic conciliar tradition attest the problem as resolved by the latria/dulia distinction. Historians of the iconoclast controversies (a body of scholarship not itself invested in either doctrinal outcome) corroborate that this was a genuine, contested theological-political problem rather than a settled matter invented after the fact — though they also document that the 'resolution' was reached only after decades of imperial coercion running in both directions, which the iconodule tradition's own self-narrative underplays.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, contested).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the reading does not extract material rent from a victim population; its costs are borne mainly in interpretive labor (theological policing of proper depiction) rather than transfer from a coerced payer class. Suppression is moderate-low (0.22): the boundary between acceptable veneration and forbidden worship is policed by doctrine and social/ecclesiastical sanction rather than by systematic coercive enforcement against dissenters, though historically image-venerators were persecuted under the rival iconoclast state apparatus (that persecution belongs to the SIBLING constraint's victim set, not this one's). Theater ratio is low and roughly flat (0.10-0.15): the conciliar and catechetical apparatus that maintains the distinction performs real doctrinal work, not mere performance. Accessibility collapse is moderate (0.35) — believers who reject the distinction can still practice a materially minimalist or aniconic piety within the same broader tradition, so alternatives are not fully foreclosed. Resistance is moderate (0.4), reflecting the genuine and long-running theological contest this reading had to win against iconoclasm.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical hierarchy's seat this looks like settled orthodoxy grounding a stable coordination function. From an excluded iconoclast-authority seat (kept out of this reading's operative frame by construction) the same material practice looks like unrepented idolatry — that divergence is exactly why the kernel required decomposition into separate constraint files rather than one story with an internal dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Laity, painters, and monastic communities are declared beneficiaries because the reading licenses a devotional and artisanal practice they value and depend on; the ecclesiastical hierarchy is beneficiary-and-agenda-setter because it both derives unifying doctrinal authority from having settled the question and administers the ongoing boundary. No victim group is declared for THIS constraint: the historical victims of image-related persecution (destroyed icons, persecuted venerators) are the product of the iconoclast reading's enforcement, not this reading's operation — declaring them here would conflate two structurally distinct constraints and violate epsilon-invariance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling incarnational theology with the image prohibition) is treated as contested rather than resolved-and-abandoned: the doctrinal function is still actively invoked in ongoing liturgical practice, not merely inherited as inertia. This blocks a piton misreading — the low theater ratio and continued live use of the distinction in catechesis and liturgy indicate the coordination function has not atrophied into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_line_stability,
    'Is the latria/dulia distinction a stable, principled theological boundary, or does it function primarily as a post-hoc justification that shifts to accommodate whatever devotional practice has already become popular?',
    'Historical-doctrinal analysis of whether the boundary has been redrawn reactively in response to popular practice (e.g., expanding permitted veneration categories after the fact) versus applied consistently forward from fixed principle across councils and centuries.',
    'If the line is principled and stable, the Rope classification (genuine coordination function) is well-supported. If the line has repeatedly moved to ratify whatever practice already existed, the coordination story is weaker and the reading looks more like retrospective legitimation of popular piety than a load-bearing theological distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_line_stability, conceptual, 'Whether latria/dulia is a stable principle or a reactive justification.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the iconodule reading diverge from the iconoclast and moderate-iconoclast readings — is the disagreement about the metaphysics of representation (can any material image mediate the divine at all), or purely about the psychology of the venerator (can intent reliably distinguish worship from honor)?',
    'Close comparative reading of Nicaea II''s definition against the iconoclast Council of Hieria (754) and the moderate positions in Carolingian sources (Libri Carolini) to isolate whether the dispute is fundamentally metaphysical (nature of matter/Incarnation) or epistemic/psychological (reliability of the intent-based distinction).',
    'If metaphysical, this reading and the iconoclast reading are strictly incompatible in any single framework (supports a forecloses relation). If the dispute is primarily about reliably policing intent, the readings could in principle coexist as differing risk-tolerances within one framework (supports coexists_with, as declared here).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating whether the kernel dispute is metaphysical or psychological/regulatory.').

omega_variable(
    conciliar_coercion_provenance,
    'Was the Nicaea II resolution reached through genuine theological consensus, or substantially through imperial political pressure and the coercive suppression of iconoclast clergy — and does that provenance affect how much weight the ''resolved'' founding-problem status deserves?',
    'Historical review of the political circumstances surrounding Nicaea II and the subsequent second iconoclast period (815-843), including the role of imperial authority in enforcing conciliar outcomes on both sides.',
    'If the resolution was substantially imposed by imperial coercion rather than settled by theological argument alone, the founding_problem_status of ''contested'' (rather than cleanly ''live'' or ''dead'') is the more honest reading, and claims of settled orthodoxy should be tempered accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_coercion_provenance, empirical, 'Whether conciliar resolution reflects theological consensus or imposed political settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconodule_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconodule_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__iconodule_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__iconodule_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(decalogue_image_prohibition__iconodule_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the decalogue_image_prohibition kernel. iconoclast_reading treats all religious imagery as prohibited idolatry (expected Snare/Tangled-Rope profile under enforcement, with icon-venerators as victims). moderate_iconoclast_reading permits regulated two-dimensional images while forbidding statuary (an intermediate coordination structure with its own narrower victim set — sculptors, three-dimensional devotional traditions). This file (iconodule_reading) is the least extractive of the three: no enforcement-driven victim class is declared internally, since the historical persecution of icon-venerators belongs to the iconoclast reading's operation, not this one's. All three share the network edge set so contamination/coupling analysis can trace how a shift in one reading's dominance (e.g., a state adopting iconoclasm) reshapes the others' stakeholder and victim structures without merging their epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
