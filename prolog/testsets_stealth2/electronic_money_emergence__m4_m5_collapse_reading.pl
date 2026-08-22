% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: The M4/M5 Statistical Distinction as the Constituting Apparatus of 'Electronic Money' (Collapse Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   The M4/M5 statistical distinction — the boundary between broad money and
 *   the wider liquidity aggregates — is the classificatory apparatus that
 *   first gave 'electronic money' a statistical line-item, and this reading
 *   holds that the category was constituted there, retroactively, rather than
 *   recording a pre-existing historical event. On this reading there is no
 *   genuine emergence of electronic money to date: monetary practice
 *   dematerialized continuously, and the appearance of an 'electronic money'
 *   category in the aggregates marks the moment statisticians needed
 *   somewhere to put new instruments, not a threshold the instruments
 *   crossed. The arrangement under examination is the standing classification
 *   itself: a measurement convention that persists after the policy program
 *   it was built to serve (monetary targeting) was abandoned, maintained by
 *   publication inertia and comparability requirements, and imposing a
 *   diffuse epistemic cost on everyone whose work must cite official money
 *   statistics. This story is one reading of the kernel
 *   electronic_money_emergence (see kernel_context and omegas); the sibling
 *   readings — became_thinkable and first_held — are separate constraint
 *   stories with their own epsilon referents. KEY AGENTS (by structural
 *   relationship): - central_bank_statistics_departments: agenda-setter and
 *   continuity beneficiary (institutional/identity_locked) — administers the
 *   taxonomy, absorbs drift in footnotes - central_bank_policy_committees:
 *   former primary beneficiary, now vestigial (institutional/mobile) —
 *   abandoned the aggregates as operating instruments -
 *   statistical_standards_bodies: secondary beneficiary
 *   (institutional/constrained) — harmonization depends on the boundary
 *   holding still - monetary_economists: primary payer
 *   (organized/constrained) — the profession's empirical record is indexed to
 *   the taxonomy - economic_historians: payer (moderate/constrained) —
 *   periodization inherits the artifact - financial_journalists: diffuse
 *   payer (moderate/mobile) — repeat the official periodization -
 *   alternative_measurement_researchers: excluded payer
 *   (organized/constrained) — behavior-based taxonomies lack official
 *   standing - measurement_historians: analytical observer — no stake in
 *   which taxonomy wins
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.28).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "The M4/M5 Statistical Distinction as the Constituting Apparatus of 'Electronic Money' (Collapse Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '9f8e993b-4fe3-43c1-be36-35913237d0d4').
narrative_ontology:cs_kernel_codification('9f8e993b-4fe3-43c1-be36-35913237d0d4', distributed).
narrative_ontology:cs_authority_grounding('9f8e993b-4fe3-43c1-be36-35913237d0d4', practice).
narrative_ontology:cs_interpretation_layer_present('9f8e993b-4fe3-43c1-be36-35913237d0d4').
narrative_ontology:cs_reading_relation('9f8e993b-4fe3-43c1-be36-35913237d0d4', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('9f8e993b-4fe3-43c1-be36-35913237d0d4', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('9f8e993b-4fe3-43c1-be36-35913237d0d4', foundational, no_emergence_event_independent_of_measurement).
narrative_ontology:cs_axiom_status(no_emergence_event_independent_of_measurement, holdable).
narrative_ontology:cs_axiom_grounding('9f8e993b-4fe3-43c1-be36-35913237d0d4', no_emergence_event_independent_of_measurement, empirically_contingent).
narrative_ontology:cs_axiom('9f8e993b-4fe3-43c1-be36-35913237d0d4', foundational, statistical_categories_constitute_not_record).
narrative_ontology:cs_axiom_status(statistical_categories_constitute_not_record, holdable).
narrative_ontology:cs_axiom_grounding('9f8e993b-4fe3-43c1-be36-35913237d0d4', statistical_categories_constitute_not_record, empirically_contingent).
narrative_ontology:cs_axiom('9f8e993b-4fe3-43c1-be36-35913237d0d4', secondary, aggregate_taxonomy_decoupled_from_monetary_practice).
narrative_ontology:cs_axiom_status(aggregate_taxonomy_decoupled_from_monetary_practice, holdable).
narrative_ontology:cs_axiom_grounding('9f8e993b-4fe3-43c1-be36-35913237d0d4', aggregate_taxonomy_decoupled_from_monetary_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('9f8e993b-4fe3-43c1-be36-35913237d0d4', classificatory_convention_without_event_referent).
narrative_ontology:cs_drift_state('9f8e993b-4fe3-43c1-be36-35913237d0d4', contemporary_post_dematerialization, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('9f8e993b-4fe3-43c1-be36-35913237d0d4', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_policy_committees).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, statistical_standards_bodies).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, economic_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, financial_journalists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_measurement_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, central_bank_policy_committees).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, comparability_over_validity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile and publish the monetary aggregate series, maintain the definitional notes that say what counts in each bucket, and decide when new instruments get reclassified between them. The department's working identity is bound up with the continuity of these series: re-drawing the boundary means declaring decades of published numbers a different object than advertised, so definitional drift is absorbed through footnotes and revision notes rather than through redrawing the line. Stopping publication is not a live option; incremental adjustment is.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, beneficiary).

% Once set interest-rate targets against growth in these aggregates; since the targeting programs were abandoned they consult the series occasionally, mostly to give data-driven policy an official number to cite. They bear a residual cost when the aggregates misdescribe the payments system they actually manage, but they have moved their operating frameworks to other indicators, so little of that cost lands on them now.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_policy_committees, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, central_bank_policy_committees, payer).

% Harmonize national monetary statistics into comparable international tables. Their work presupposes that each national taxonomy holds still; a redrawn boundary would invalidate harmonization manuals and cross-country series. They gain continuity from the convention's persistence and would bear large coordination costs from its revision, so they press for stability in harmonization meetings.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, statistical_standards_bodies, beneficiary,
    institutional, generational, constrained, global).

% Build models and publish empirical work on money and payments. The official aggregates are the data with standing — citing them is required for publication and policy relevance — so the profession's empirical record is indexed to categories that track balance-sheet presentation rather than payment behavior. Economists who study payments directly must either translate findings into the official taxonomy or accept lower visibility; many also draw publication capital from the aggregates' long time series.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists, beneficiary).

% Periodize the history of money using the archived statistical record, which is organized by the official taxonomy. The category of electronic money enters their narratives with the date the statistical line-item appeared, and correcting for the artifact requires original archival work against the grain of every published series. The archive they depend on is itself the thing they would need to correct.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, economic_historians, payer,
    moderate, generational, constrained, global).

% Translate monetary statistics for the public and repeat the official periodization — electronic money 'emerged' when the statistics say it did. The cost to them is small and diffuse: an occasionally wrong story. They can drop the frame at any time without penalty, and mostly do not notice it as a frame.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_journalists, payer,
    moderate, immediate, mobile, global).

% Map payments at the transaction level — card networks, real-time settlement systems, stablecoin flows — and propose classifications built from payment behavior rather than balance-sheet position. They sit outside the official statistical process: consulted rarely, funded precariously, and unable to give their series official standing, because official standing is reserved for the taxonomy they are proposing to replace. Their work is citable but not authoritative.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_measurement_researchers, excluded,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, alternative_measurement_researchers, payer).

% Study how statistical categories are drawn, stabilized, and abandoned across domains. They take testimony from every other seat and have no stake in which taxonomy wins; their analyses of the monetary aggregates follow the same pattern they document in census categories and unemployment definitions.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, measurement_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: When drawn, the M4/M5 boundary let statistical offices, reporting banks, and policy committees mean the same thing by 'broad money': a fixed list of balance-sheet items summed into a single number, comparable across institutions and over time. It solved the problem of making money measurable at all while instruments were proliferating — a shared definition instead of per-institution judgment calls.
% TRANSFER_FUNCTION: Moves classification labor and epistemic standing. Reporting institutions must present balance sheets in the taxonomy's categories; analysts, journalists, and historians must cite the official aggregates for their work to count; attention and authority flow to the published series and away from transaction-level or behavior-based descriptions of money. What moves is deference to the official cut, from everyone who studies money, to the statistical apparatus that draws it.
% ABSENT_VOICES: Transaction-level payments researchers and behavior-based classification proposals are outside the official process — they would object that the boundary sorts by balance-sheet presentation rather than by what money does, but they hold no seat in the statistical committees that maintain the definitions. Also absent: any representative of the pre-taxonomy practice the category was retroactively imposed on; the record was rewritten in the taxonomy's terms before anyone thought to object.
% DISAPPEARANCE_RATIONALE: Publication calendars, international harmonization tables, and every time series indexed to the aggregates would break at once; the statistical apparatus would have to re-base or discontinue the family, and the historiography built on the official periodization would lose its spine. Monetary practice itself — what settles payments, what banks owe — would not rearrange at all, which is precisely this reading's claim: the arrangement holds up the measurement layer, not the money.
% FOUNDING_PROBLEM: Monetary targeting. The distinction was drawn so that authorities could define, measure, and aim to control a quantity of money — monetarist policy needed an aggregate stable enough to target and broad enough to bind, and the proliferation of near-money instruments demanded a principled line between money and other liquid assets.
% FOUNDING_PROBLEM_CORROBORATION: The central banks' own record attests it: targeting frameworks were formally abandoned (the Bank of England dropped its monetary targets in the mid-1980s; the Federal Reserve retired M3 in 2006 after stating the aggregates no longer had a stable relationship with policy goals), and the post-mortem literature — Goodhart's critique of aggregate stability foremost — comes from economists who bore the taxonomy's costs rather than its continuity benefits. No party outside the statistical apparatus attests that the targeting problem is still live.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and diffuse: the arrangement's cost is epistemic — a category scheme that sorts by balance-sheet presentation rather than payment behavior, imposed on everyone whose work needs official numbers — with no seat capturing that cost as gain. Suppression is low (0.28): nothing coerces; official standing is simply reserved for the taxonomy, and alternative series remain publishable though not authoritative. The enforcement picture is static across the interval — inertial persistence, not an enforcement ratchet — so no suppression_requirement series is authored; the scalar carries the picture. Theater is high (0.62) and rising: aggregates are still compiled, revised, and harmonized long after their operating use atrophied, and definitional maintenance is increasingly footnotes defending the boundary rather than measurement serving policy. Accessibility collapse is low (0.35): behavior-based alternatives exist and are known; they are crowded out of official standing, not collapsed. Resistance is low-moderate (0.30): the policy committees' abandonment and the Goodhart-line critique register real resistance, but neither threatens the series' publication. The 2008 theater dip is a crisis-driven temporary revival of aggregate attention (QE-era money-creation debates) — a side effect of external events, not an oscillation mechanism. Base extractiveness rises monotonically as dematerialization widens the gap between taxonomy and practice, plateauing after 2020 as re-basing exercises partially caught up. Both series run on one shared time grid; the final values match the authored base_properties.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the statistics departments' position the arrangement is their own working craft — a boundary they maintain, revise, and defend, experienced as continuity rather than imposition (identity_locked exit means they cannot simply stop). From the policy committees' position it is a retired instrument kept on the shelf for citation. From the payers' position it is an imposed vocabulary: economists must index to it for standing, historians must labor against it to periodize honestly, and the excluded researchers experience it as a closed door — their classifications are citable but never authoritative. Same domain, same official numbers, four different lived structures; the engine computes this divergence from power, exit, and role, and the divergence — not any single seat's report — is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations are continuity-benefits, not capture: the departments and standards bodies gain mandate persistence and harmonization stability, which is why they press for the boundary to hold still, but neither receives the misdescription cost as gain — hence gain_flow is affirmatively 'diffuse'. The payers bear the taxonomy's epistemic cost in proportion to how bound their work is to official standing: historians and the excluded researchers sit nearest the target end (constrained exit; the archive itself is structured by the taxonomy); economists sit slightly lower (they also draw publication capital from the long series); journalists sit nearest symmetric (mobile exit, trivial stakes). Policy committees have largely exited — their directionality sits near the beneficiary end with little residual cost, which is exactly the atrophied-benefit profile that distinguishes this arrangement from an enforced one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a measurable, targetable quantity of money — is dead: targeting was abandoned, the aggregates lost their stable relationships, and the apparatus itself retired series. What persists is the classificatory shell. Reading this as a rope would flatter it (the comparability function is real but vestigial and no longer the reason anything happens); reading it as a snare would overstate it (no seat captures the cost; nothing is coerced). The piton reading keeps both errors out: an atrophied function maintained by inertia, its costs too diffuse for any bearer to organize a fix (fixing_cost: prohibitive — re-drawing the boundary invalidates decades of comparable series, harmonization manuals, and every study indexed to the aggregates) and its continuity-benefits too modest to motivate active defense. The R5 record — dead founding problem paired with a world_rearranges verdict — flags the zombie structure for the engine's mismatch check.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_event_status,
    'Does ''electronic money'' name a genuine historical emergence event (as the became_thinkable and first_held sibling readings of kernel electronic_money_emergence hold), or is it a category constituted retroactively by the M4/M5 statistical distinction, with no event to mark?',
    'Comparative periodization: test whether monetary practice (settlement media, payment instrument mix, dematerialization of balances) shows a discontinuity at the date the statistical category appears, or only at dates the taxonomy does not mark. If practice is smooth across the category''s birth date, the artifact reading is confirmed.',
    'If a genuine event exists independent of the taxonomy, this reading collapses and the arrangement under study becomes a threshold (the sibling readings'' terrain) rather than a measurement convention; the epsilon referent shifts from the classification apparatus to the emergence event itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_event_status, empirical, 'Whether the electronic-money category marks a real event or is a statistical artifact.').

omega_variable(
    sibling_reading_structural_delta,
    'What would adopting a sibling reading (became_thinkable_reading or first_held_reading) change structurally in this story''s constraint set?',
    'Author the sibling stories: under became_thinkable the arrangement is the conceptual-possibility threshold (a structural limit on when dematerialized money could exist); under first_held it is an institutional first-instantiation claim (a dated historical fact with evidentiary coordination function). This story''s classificatory apparatus would then become downstream evidence rather than the constituting structure.',
    'Sibling adoption relocates the cost: the measurement convention becomes a possibly distorting lens on a real event rather than the constituting structure, lowering its extractiveness toward a stale-instrument profile and changing the stakeholder set (technologists and first institutional bearers enter; statisticians recede).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta if a sibling reading of the emergence kernel were adopted instead.').

omega_variable(
    artifact_vs_blurry_referent,
    'Is the M4/M5 boundary wholly artifactual (no underlying monetary structure corresponds to it), or does it track a real but blurry transition that the taxonomy merely mis-dates and over-sharpens?',
    'Instrument-level audit: classify each instrument the aggregates sweep in by its actual payment-function and store-of-value characteristics at introduction; if the M4/M5 line consistently separates instruments by real function, the boundary has a referent.',
    'A real referent reclassifies the arrangement from piton toward a degraded rope (a stale but once-true convention); a pure artifact confirms the piton reading and the retroactive-emergence claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_vs_blurry_referent, empirical, 'Whether the statistical boundary has any underlying monetary referent.').

omega_variable(
    performativity_direction,
    'Did the statistical category merely label pre-existing practice, or did it performatively shape practice (institutions designing instruments and presenting balance sheets to fit the categories)?',
    'Product-design and reporting-instruction archives: look for instrument design decisions and balance-sheet presentation choices made to land in the favored aggregate bucket after the category''s introduction.',
    'If performative, the distinction had real constitutive power, raising its functional significance and complicating the ''no genuine emergence'' claim (the category would have helped cause what it claims only to measure); if inert, the piton reading stands cleanly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performativity_direction, empirical, 'Whether the category shaped practice or only recorded it.').

omega_variable(
    inertia_vs_gatekeeping,
    'Is the convention''s persistence purely inertial, or does official-data gatekeeping (comparability requirements, funding tied to official series) actively suppress alternative taxonomies?',
    'Track funding and publication outcomes for alternative measurement programs; test whether proposals to re-base the official aggregates die from indifference or from active comparability objections.',
    'Substantial gatekeeping would raise suppression and drift the classification toward enforced maintenance; pure inertia confirms the piton reading with the authored low suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_gatekeeping, empirical, 'Whether persistence is inertial or actively enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1982, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement_basis(elec_tr_t1982, observed).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).
narrative_ontology:measurement(elec_tr_t1997, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1997, 0.4).
narrative_ontology:measurement_basis(elec_tr_t1997, observed).
narrative_ontology:measurement(elec_tr_t2006, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement_basis(elec_tr_t2006, observed).
narrative_ontology:measurement(elec_tr_t2008, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2008, 0.47).
narrative_ontology:measurement_basis(elec_tr_t2008, observed).
narrative_ontology:measurement(elec_tr_t2015, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2015, 0.56).
narrative_ontology:measurement_basis(elec_tr_t2015, observed).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.59).
narrative_ontology:measurement_basis(elec_tr_t2020, observed).
narrative_ontology:measurement(elec_tr_t2024, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(elec_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1982, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1982, 0.2).
narrative_ontology:measurement_basis(elec_be_t1982, observed).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.26).
narrative_ontology:measurement_basis(elec_be_t1990, observed).
narrative_ontology:measurement(elec_be_t1997, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1997, 0.31).
narrative_ontology:measurement_basis(elec_be_t1997, observed).
narrative_ontology:measurement(elec_be_t2006, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2006, 0.35).
narrative_ontology:measurement_basis(elec_be_t2006, observed).
narrative_ontology:measurement(elec_be_t2008, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(elec_be_t2008, observed).
narrative_ontology:measurement(elec_be_t2015, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(elec_be_t2015, observed).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(elec_be_t2020, observed).
narrative_ontology:measurement(elec_be_t2024, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(elec_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_targeting_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the emergence of electronic money' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon referents — a conceptual-thinkability threshold (became_thinkable_reading), an institutional first-instantiation event (first_held_reading), and a measurement-artifact claim about the classificatory apparatus itself (this story). Each is authored as its own constraint story with its own beneficiaries, victims, and classification; they are linked here because the artifact claim, if sustained, removes the evidentiary ground on which the sibling periodizations rest — the taxonomy's authority is upstream of the siblings' dating evidence. monetary_aggregate_targeting_regime is the upstream arrangement this apparatus was built to serve; its abandonment is this story's mandatrophy origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
