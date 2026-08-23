% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition — Moderate Iconoclast Regime (Statuary Forbidden, Flat Images Licensed)
 *   domain: theology/religious authority/visual culture
 *
 * SUMMARY:
 *   A Decalogue-confessing community regulates material mediation in devotion
 *   through a differentiated image regime: carved and rounded sacred figures
 *   are categorically forbidden on the ground that dimensional representation
 *   presents the highest drift into idolatry, while painted and printed flat
 *   images are lawful only after certification — preapproved designs,
 *   workshop registration, periodic inspection, and review fees. The regime
 *   is taught internally as double fidelity: obedience to the commandment's
 *   letter and prudence against abuse. Operationally it maintains a permanent
 *   tribunal and inspectorate, a licensable category of approved
 *   iconographers, and a reporting expectation on the laity; statuary
 *   workshops are closed, their tools subject to confiscation, and their
 *   practitioners pushed into secular trades, emigration, or clandestine
 *   work. KEY AGENTS (by structural relationship): -
 *   ecclesiastical_image_authority: Agenda-setter (institutional/mobile) —
 *   drafts the flat/round boundary, licenses workshops, convenes tribunals,
 *   collects fees and deference - image_inspection_officials:
 *   Beneficiary-administrator (organized/constrained) — salaried inspectors
 *   whose posts exist because the inspection program does - sacred_sculptors:
 *   Primary payer (moderate/identity_locked) — trade rendered contraband;
 *   craft identity binds them to the forbidden form - licensed_icon_painters:
 *   Payer with secondary benefit (moderate/constrained) — certified practice
 *   purchased through fees, preapproval, and inspection - devout_laity: Payer
 *   with secondary benefit (moderate/constrained) — keep approved flat icons,
 *   surrender figured objects, report infractions when asked -
 *   statuary_advocacy_factions: Excluded (organized/trapped) — hold the
 *   flat/round line arbitrary; no seat in the synod; dissent only privately -
 *   civil_magistrates: Observer (institutional/analytical) — adjudicate
 *   disputes, occasionally execute confiscations, watch for unrest
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.66).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.7).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition — Moderate Iconoclast Regime (Statuary Forbidden, Flat Images Licensed)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious authority/visual culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '2f0982b2-08d8-4cf9-b492-78a18507cb6e').
narrative_ontology:cs_kernel_codification('2f0982b2-08d8-4cf9-b492-78a18507cb6e', fixed_text).
narrative_ontology:cs_authority_grounding('2f0982b2-08d8-4cf9-b492-78a18507cb6e', extraction).
narrative_ontology:cs_interpretation_layer_present('2f0982b2-08d8-4cf9-b492-78a18507cb6e').
narrative_ontology:cs_reading_relation('2f0982b2-08d8-4cf9-b492-78a18507cb6e', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('2f0982b2-08d8-4cf9-b492-78a18507cb6e', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('2f0982b2-08d8-4cf9-b492-78a18507cb6e', foundational, round_figures_categorical_idolatry_hazard).
narrative_ontology:cs_axiom_status(round_figures_categorical_idolatry_hazard, holdable).
narrative_ontology:cs_axiom_grounding('2f0982b2-08d8-4cf9-b492-78a18507cb6e', round_figures_categorical_idolatry_hazard, instrumental).
narrative_ontology:cs_axiom('2f0982b2-08d8-4cf9-b492-78a18507cb6e', foundational, regulated_flat_imagery_lawful).
narrative_ontology:cs_axiom_status(regulated_flat_imagery_lawful, holdable).
narrative_ontology:cs_axiom_grounding('2f0982b2-08d8-4cf9-b492-78a18507cb6e', regulated_flat_imagery_lawful, theological).
narrative_ontology:cs_reference_frame('2f0982b2-08d8-4cf9-b492-78a18507cb6e', covenant_flat_round_boundary).
narrative_ontology:cs_drift_state('2f0982b2-08d8-4cf9-b492-78a18507cb6e', contemporary_enforcement_record, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f0982b2-08d8-4cf9-b492-78a18507cb6e', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_image_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, image_inspection_officials).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, sacred_sculptors).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, materially_indexed_prohibition_jurisprudence).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, prudential_image_oversight_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and revises the boundary between forbidden rounded figures and lawful flat images; registers workshops, approves designs before execution, convenes review tribunals, and disciplines violations through censure and confiscation orders. Certification fees and review charges flow into its courts, and every approved icon and every prosecuted infraction demonstrates the office's necessity. It could redraw or retire the standard at its own discretion, and its standing in the confession rests substantially on administering this boundary.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_image_authority, agenda_setter,
    institutional, generational, mobile, continental).

% Salaried visitors who tour registered workshops, examine finished pieces against the flatness and figure-completeness rules, stamp certifications, and file violation reports. Their posts, rank, and stipends exist only because the inspection program does; reassignment outside the apparatus means abandoning accumulated seniority and specialist standing. Day to day they also set the practical enforcement tempo — which workshops get visited, and what counts as a marginal case.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, image_inspection_officials, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, image_inspection_officials, agenda_setter).

% Trained carvers of liturgical and devotional figures whose trade the ban renders contraband. Commissions vanish, unfinished works and tools are liable to seizure, and the paths out are bitter ones: secular subjects, emigration to jurisdictions without the ban, or secret work at the risk of fines and confiscation. Apprenticeship lineage, guild formation, and the conviction that figured carving is their vocation bind them to the forbidden form even where secular alternatives technically exist.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, sacred_sculptors, payer,
    moderate, biographical, identity_locked, regional).

% Painters of flat devotional panels and illuminated manuscripts who may sell only after registering, submitting designs for preapproval, paying certification and renewal fees, and accepting inspection. Compliance consumes time and margin, and a failed review can idle a season's output. At the same time the ban removes the carvers' competition and shuts out uncertified rivals, so licensure doubles as a market shield — a protected category they pay to remain inside.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_painters, beneficiary).

% Households that keep approved flat icons, surrender or destroy figured objects when inspectors call, and are expected to report neighbors' statuary. They lose the devotional forms centered on carved figures — household shrines, votive statues — and absorb the atmosphere of surveillance in exchange for a guaranteed-orthodox channel of image devotion. Leaving the confession altogether is the main exit, and it costs kinship, burial, and belonging.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity, beneficiary).

% Monastics, scholars, and craft families who maintain that the flat/round line is arbitrary — that carved figures served the sanctuary in the tradition's own past, or that the oversight apparatus distorts devotion more than images ever endangered it. They hold no seat in the synod that drew the line, circulate arguments privately, and face censure if they press openly; remaining inside the confession while dissenting is their only posture, since departure surrenders the very community the dispute is about.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, statuary_advocacy_factions, excluded,
    organized, generational, trapped, continental).

% Civil officers who adjudicate disputes between the tribunals and workshops, execute occasional confiscation writs, and monitor whether enforcement breeds unrest or flight. They take testimony from every seat and can decline cooperation, which makes them a check the authority must court rather than command.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, civil_magistrates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_image_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single enforceable boundary standard telling scattered congregations which material forms of devotion are lawful — solving the problem of inconsistent local practice and giving makers a determinate rule for what may be produced.
% TRANSFER_FUNCTION: Moves certification fees, inspection compliance labor, preapproval submissions, and doctrinal deference from sculptors, icon painters, and laity to the central authority and its salaried inspectors; it also transfers legitimacy — every certified icon demonstrates the authority's indispensability.
% ABSENT_VOICES: Sculptors whose trade the ban annihilates had no seat in the drafting synod; monastics and scholars who regard the oversight apparatus itself as the distortion were excluded; lay devotees attached to household figures learned the rule only as enforcement arrived. They persist today as the statuary_advocacy_factions seat — present in the confession, absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the ban and its apparatus vanished overnight, statuary workshops would reopen within seasons, the tribunals and inspectorate would dissolve along with their fee income, the licensure cartel around flat images would collapse, and devotional practice would immediately diversify back toward carved figures — the authority would lose both the revenue and the office the boundary sustains.
% FOUNDING_PROBLEM: The commandment against graven images posed a standing hazard: material representations in devotion tend to acquire cultic treatment, and the community needed a rule for honoring the prohibition without abolishing every image.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: devout laity and non-officeholding clergy independently attest fear of idolatry drift as a standing hazard, and neighboring confessions record parallel anxieties. However, the specific load-bearing claim — that rounded figures carry categorically higher risk than regulated flat images — is attested almost exclusively by the regulatory authority and the tribunals it staffs; no independent source confirms the differential, and lay practice reliably returns to figured devotion wherever enforcement lapses.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66) is high because the regime's costs are imposed on practice regardless of abuse: an entire craft sector loses its licit economic base outright, and permissible practice carries a permanent compliance tax of fees, preapproval delay, and inspection exposure. Suppression (0.70) is higher because the ban's persistence depends on active machinery — tribunals, confiscations, workshop raids, and a lay reporting expectation — rather than on voluntary assent; clandestine carving resurges whenever enforcement relaxes. Theater ratio (0.35) reflects an inspection program that performs real screening but increasingly accumulates ceremonial review steps, renewal rituals, and marginal-case hearings that persist long after the urgency that created them. Accessibility collapse (0.58) is partial: the sculptor's licit path is fully closed, the painter has exactly one licensed route, but the laity retain the sanctioned flat-image channel, so alternatives narrow without disappearing. Resistance (0.62) is substantial and chronic — hidden workshops, protected factions, and periodic open controversy.
 *   
 *   CYCLICAL DYNAMICS: the series oscillates with a ratchet. Abuse scandals and discovered clandestine shrines trigger crackdown waves (peaks at T3, T9, T15, T21, T27 in suppression_requirement); enforcement fatigue and pastoral complaint then drive relaxation (troughs at T6, T12, T18, T24). Each wave leaves permanent institutional additions — a new fee schedule, an expanded inspectorate, registry requirements — so every trough sits higher than the last (0.47 to 0.53 to 0.58 to 0.62) and every peak climbs (0.61 to 0.66 to 0.71 to 0.73 to 0.75). The oscillation is itself an extraction mechanism (intermittent reinforcement): relaxation normalizes the prior peak as the baseline, and each crackdown monetizes renewed alarm. Theater rises steadily across the cycle because ceremonial review accretes in calm phases and is never dismantled in strict ones. Base properties are authored at T=30, the early-relaxation phase just past the fifth peak. All three tracked metrics share one eleven-point grid (T0–T30, step 3), each authored at every point. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the continental scope of the standard.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply and the engine should compute different types from each. From the authority seat the regime is faithful administration of a received commandment plus prudent safeguarding — a coordination structure it built and staffs. From the sculptor seat the same structure is the annihilation of a vocation, harsher than any mere tax. From the painter seat it is a burden offset by a cartel benefit, and from the laity seat both protection and surveillance at once. Because the collecting seat is singular and the paying seats are plural and differently positioned, per-seat computation should return extraction-weighted classifications for sculptors, painters, and laity and a coordination-weighted classification for the authority; the divergence is the finding, and the authored claim does not reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   The authority and its inspectors sit at the beneficiary pole: fees, salaries, and status route to them, and they control the standard's content. Sacred sculptors sit nearest the full-target pole — they receive nothing from the arrangement and bear its total cost — and their identity_locked exit (apprenticeship lineage, guild formation, vocational conviction binding them to the craft) pushes them deeper toward the target end than their formal moderate power alone would suggest. Licensed icon painters and devout laity carry mixed declarations: both bear compliance costs and lost devotional forms while receiving certified channels, so their derived directionality lands mid-range, pulled toward the payer side by constrained exit. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already separate the poles, and the dual-positioned seats are handled by their paired declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Against mislabeling as rope: the genuine coordination residue — a uniform published boundary standard — could exist as doctrine alone, without tribunals, fees, or inspectors; what distinguishes this arrangement is that its persistence mechanism is enforcement against recurrent demand, which is the signature of a structure held up by coercion rather than consensus. Against mislabeling as piton: the function is not atrophied and enforcement actively bites in every crackdown phase, and a concentrated beneficiary (the authority) demonstrably profits from maintenance — a profile the piton cell excludes. Against mislabeling as tangled_rope: although the arrangement coordinates and extracts at once, the coordination content (a rulebook) is separable from the coercive apparatus that constitutes the constraint's actual persistence mechanism, whereas a tangled rope's coordination and extraction are structurally inseparable. No mandatrophy declaration is authored because the founding problem remains live inside the tradition; the regime is neither a sunset scaffold nor a theatrical remnant but an actively enforced arrangement whose costs and benefits are structurally asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the decalogue_image_prohibition kernel (moderate_iconoclast_reading). Which structural features change under the sibling readings?',
    'Compare the compiled sibling files: decalogue_image_prohibition__iconoclast_reading extends the prohibition to all material representation (the victim set expands to every image-maker and the licensed flat-image economy disappears); decalogue_image_prohibition__iconodule_reading dissolves the oversight apparatus as unjustified (honor through images is lawful and the compliance surface vanishes). The disagreement is located in the prohibition''s extension and in whether misuse or mediation itself is the hazard.',
    'If the iconoclast sibling prevails, this constraint merges into total prohibition with higher epsilon and a broader victim set; if the iconodule sibling prevails, the constraint collapses into a teaching norm with near-zero enforcement extraction. This file classifies only the moderate split and hedges neither way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame position: one reading of a contested kernel; sibling deltas recorded here rather than folded into the classification.').

omega_variable(
    flat_round_risk_differential,
    'Does three-dimensional statuary in fact carry categorically higher idolatry risk than regulated two-dimensional images?',
    'Comparative study of devotional drift (votive treatment of images, cultic accretion around figured objects) across communities with differing image regimes, controlling for enforcement intensity.',
    'If no reliable differential exists, the categorical ban is gatekeeping without protective yield and the snare reading strengthens; if a real differential exists, part of the ban''s cost is the price of genuine protection and the excess-extraction attribution narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flat_round_risk_differential, empirical, 'Whether the flat/round risk differential that justifies the split is empirically real.').

omega_variable(
    certification_fee_rent_share,
    'What share of certification fees, review charges, and inspection levies funds the tribunals'' operating cost versus accruing as surplus to the authority?',
    'Fee-schedule audit against tribunal budgets and official compensation, benchmarked against equivalent private authentication and quality-assurance costs.',
    'A large surplus share confirms that the receipt flow accrues to the authority seat; cost-recovery parity would soften the extraction attribution toward ordinary coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_fee_rent_share, empirical, 'Whether the compliance levy is rent or cost recovery.').

omega_variable(
    enforcement_cycle_mechanism,
    'Is the recurring crackdown-relaxation cycle an internal intermittent-reinforcement mechanism that ratchets the apparatus, or an exogenous response to genuine waves of abuse?',
    'Track inspectorate headcount, fee introductions, and permanent institutional additions across cycle phases; an internal mechanism predicts monotonic apparatus growth despite oscillating enforcement intensity.',
    'If internal, the oscillation is itself the extraction engine and the rising suppression troughs are structural rather than incidental; if exogenous, the regime is responding to real hazards and the ratchet reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cycle_mechanism, empirical, 'Whether the measured enforcement cycle is endogenous ratchet or exogenous response.').

omega_variable(
    compliance_interiorization,
    'Is lay and artisanal compliance sustained by sanctions (structural) or by interiorized conviction that figured objects are spiritually dangerous (internalized)?',
    'Observe practice where enforcement visibly lapses: rapid resumption of statuary use indicates structural compliance; continued abstention indicates interiorized suppression that would outlive the apparatus.',
    'Interiorization raises effective suppression above the structural measure and predicts persistence after any deregulation; purely structural compliance predicts immediate devotional diversification upon removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_interiorization, empirical, 'Structural versus internalized suppression mechanism in the payer seats.').

omega_variable(
    authority_grounding_framing,
    'Is the regime''s authority best framed as extraction-grounded gatekeeping (the apparatus benefits from kernel stability) or as lineage-faithful jurisprudence executing a received text?',
    'Test whether the authority''s rulings track textual exegesis or institutional interest across cases where the two diverge — for example, rulings that expand licensable categories during revenue shortfalls.',
    'Under a lineage framing the same structure computes closer to a duty-executing coordination regime (tangled_rope territory); under the extraction framing the snare classification holds. The declared extraction framing follows the operative incentive structure visible in the fee and enforcement record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination in the commitment-system classification of the authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(deca_tr_t3, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(deca_tr_t6, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(deca_tr_t9, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 9, 0.26).
narrative_ontology:measurement(deca_tr_t12, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(deca_tr_t18, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement(deca_tr_t21, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 21, 0.32).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(deca_tr_t27, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 27, 0.34).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(deca_be_t3, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(deca_be_t6, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(deca_be_t9, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 9, 0.54).
narrative_ontology:measurement(deca_be_t12, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(deca_be_t18, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(deca_be_t21, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 21, 0.61).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(deca_be_t27, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 27, 0.64).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(deca_su_t3, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(deca_su_t6, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(deca_su_t9, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement(deca_su_t12, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(deca_su_t18, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(deca_su_t21, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 21, 0.73).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(deca_su_t27, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 27, 0.75).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the image prohibition' covers three structurally distinct constraints sharing one kernel text. The moderate reading (this file) splits the extension: total cost for sculptors, compliance overhead for painters and laity, gatekeeping rents for the authority. The iconoclast sibling widens the victim set to every maker of religious imagery and deletes the licensed-flat-image economy; the iconodule sibling deletes the oversight apparatus as unjustified and leaves a teaching norm. The epsilon values differ widely across the family, which is why the label decomposes. Influence flow: the stricter iconoclast reading is upstream (cited as textual warrant when enforcement tightens), and the iconodule reading is the downstream challenger whose arguments the apparatus exists to police; the moderate regime's licensing categories and inspection precedent shape the terrain on which both siblings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
