% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade Reading: Apex-Installed Commitments Requiring Fringe Validation to Stabilize
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   New commitments installed at the apex of a state cascade downward, but
 *   they do not stabilize on installation alone: they require fringe actors
 *   to adapt them into local idioms and vouch for them before adherence
 *   holds. This story instantiates the hybrid_cascade_reading of the
 *   state_commitment_installation_mechanism kernel as a clean,
 *   epsilon-invariant constraint; the sibling readings
 *   (endogenous_climb_reading, exogenous_imposition_reading) are separate
 *   stories with their own epsilon values and are linked, not averaged, here.
 *   The epsilon referent is the standing hybrid-cascade arrangement itself as
 *   this reading sees it — the two-phase installation-and-validation
 *   structure under contest — never the pure-imposition or pure-climb
 *   arrangements this reading argues against. Claim and metrics are
 *   independent authored facts: the claimed type is tangled_rope because the
 *   structure possesses BOTH a genuine coordination function (two-phase
 *   adoption solves what neither pole can) AND asymmetric appropriation
 *   (validation labor booked as assent, resistance absorbed rather than
 *   honored, adaptation costs externalized); the metrics are authored as
 *   descriptively true of the mechanism's operation, and any divergence
 *   between claim and computed type is data, not error. Assumptions: the
 *   abstract interval 0-24 models the recurring lifecycle of
 *   early-modern-style installation episodes (promulgation, brokerage,
 *   stabilization) rather than one dated case; provenance commit hashes are
 *   session placeholders.
 *
 * KEY AGENTS:
 *   - apex_state_reformers: agenda-setting collector (institutional/constrained) — installs commitments at the apex and collects durability and uniformity
 *   - fringe_cultural_brokers: dual-positioned validators (moderate/identity_locked) — collect standing for adapting the commitment, bear the adaptation labor and the post-stabilization discard
 *   - mid_level_administrators: transmission-belt collectors (organized/constrained) — convert reported uptake into careers and budgets
 *   - peripheral_communities: primary targets (powerless/trapped) — bear rewritten obligations and re-labeled dissent
 *   - rival_tradition_holders: displaced custodians (organized/trapped) — bear dispossession without a seat in the interpretation conversation
 *   - comparative_historians: analytical observers (analytical/analytical) — see the full two-phase structure and its failure modes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.46).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade Reading: Apex-Installed Commitments Requiring Fringe Validation to Stabilize").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, 'f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd').
narrative_ontology:cs_kernel_codification('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', distributed).
narrative_ontology:cs_authority_grounding('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', expertise).
narrative_ontology:cs_interpretation_layer_present('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd').
narrative_ontology:cs_reading_relation('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', state_commitment_installation_mechanism__exogenous_imposition_reading, forecloses).
narrative_ontology:cs_reading_relation('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', state_commitment_installation_mechanism__endogenous_climb_reading, influences).
narrative_ontology:cs_axiom('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', foundational, apex_initiation_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(apex_initiation_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', apex_initiation_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', foundational, fringe_validation_constitutive_of_durability).
narrative_ontology:cs_axiom_status(fringe_validation_constitutive_of_durability, holdable).
narrative_ontology:cs_axiom_grounding('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', fringe_validation_constitutive_of_durability, empirically_contingent).
narrative_ontology:cs_axiom('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', secondary, local_interpretation_counts_as_adoption).
narrative_ontology:cs_axiom_status(local_interpretation_counts_as_adoption, holdable).
narrative_ontology:cs_axiom_grounding('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', local_interpretation_counts_as_adoption, conventional).
narrative_ontology:cs_reference_frame('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', apex_seeded_fringe_validated_order).
narrative_ontology:cs_drift_state('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', contemporary_microhistorical_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f6743e1f-f8e0-43c9-bc2b-4421bf1a32fd', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_reformers).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_cultural_brokers).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_tradition_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_cultural_brokers).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, two_phase_adoption_doctrine).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, localized_interpretation_as_fulfillment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates new doctrinal, legal, and administrative commitments from the center and seeds them through appointed offices, with enforcement reach far thinner than the mapped territory. Its operating bet is that commitments planted at the top will be taken up, adapted, and vouched for below, so that localized versions count as fulfillment rather than deviation. It collects durable, roughly uniform adherence without garrisoning every district, and it keeps sole discretion over which local readings get certified.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_reformers, agenda_setter,
    institutional, generational, constrained, continental).

% Local clergy, gentry, literati, and guild heads who take the center's newly installed commitment and render it in the local idiom — adjusting rite, procedure, and vocabulary so it fits inherited practice. The center records these adaptations as validation and pays in recognition, office, and protection. The work is real labor, their standing comes to depend on remaining indispensable between center and locality, and once a commitment settles the center begins dealing with districts directly, leaving earlier brokers with titles but shrinking business.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_cultural_brokers, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_cultural_brokers, payer).

% Provincial officials, inspectors, and registrars who operate the transmission machinery: compiling adoption returns, certifying compliant districts, and disbursing rewards to cooperating local figures. Careers and office budgets scale with reported uptake, so their fortunes rise with the cascade's measured progress, and leaving the hierarchy means forfeiting rank and livelihood.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators, beneficiary,
    organized, biographical, constrained, national).

% Villages, parishes, and lineages at the receiving end. Obligations are rewritten by commitments they did not draft; negotiated adjustments are entered in ledgers as assent; refusals are logged as local variation to be worked on rather than answered. Moving beyond the jurisdiction's reach is not a live option, and the commitments follow the state's map as it extends.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities, payer,
    powerless, generational, trapped, regional).

% Custodians of the practices and doctrines the new commitments displace — old-rite specialists, customary-law elders, heterodox teachers with followings and texts. They are not consulted on how the incoming commitment will be interpreted; their counter-readings are filed as resistance to be absorbed. What they lose — standing, jurisdiction over practice, the ability to pass their tradition intact — is compensated nowhere in the arrangement.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_tradition_holders, excluded,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_tradition_holders, payer).

% Reconstruct adoption sequences across cases: which installed commitments held, which stalled, where local uptake was substantive and where it was performed for the record. They see the full two-phase shape and its failure modes, collect nothing from the arrangement, and answer to archives and peers rather than to any party in it.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_reformers).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of installing a uniform commitment across a heterogeneous territory with enforcement capacity too thin for universal coercion: apex installation supplies uniformity and speed; distributed local validation supplies fit and durability; the two phases together achieve adoption that neither pure imposition nor unplanned diffusion delivers alone.
% TRANSFER_FUNCTION: Moves legitimating labor and interpretive authority from fringe and local actors into the center's project (their adaptations are booked as assent); moves recognition, office, and protection from the center to cooperating brokers; moves binding obligations onto peripheral populations who did not draft the commitment.
% ABSENT_VOICES: Rival tradition holders and unreconciled peripherals — those whose counter-readings were classified as resistance to be absorbed rather than invited as positions. They appear in the record only as objects of absorption, never as participants; their account of what local 'validation' meant would differ sharply from the center's adoption ledgers.
% DISAPPEARANCE_RATIONALE: If the two-phase structure vanished overnight, apex-installed commitments would either stall at the center against thin enforcement or take generations to climb endogenously; broker offices and administrator certification routines disappear; peripheral obligations revert mid-stream to prior customary arrangements. Districts already adapted to the installed commitments would face a second disruption as the certification and reward machinery that held their settlements in place disappears.
% FOUNDING_PROBLEM: A low-capacity expansionist authority needed to standardize doctrinal, legal, and administrative commitments across territories whose populations were attached to local practice — faster than unplanned diffusion would deliver and cheaper than universal coercion would cost.
% FOUNDING_PROBLEM_CORROBORATION: The center's own chronicles and adoption ledgers attest the problem is live, but they sit inside the benefiting parties. Corroboration from outside the beneficiary set: parish and village records documenting adaptations negotiated before entry into official registers; comparative-historical studies of commitments that reached comparable durability through purely endogenous climbing elsewhere; and the post-stabilization discard of broker cohorts, which only makes sense if the founding problem had closed for those episodes. No party outside the beneficiary set attests that the problem remains live in its original form.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 is substantial but bounded: genuine reciprocity (brokers paid in standing, communities receiving locally fitted commitments rather than raw imposition) rides atop real appropriation — the fringe's legitimating labor is booked as assent, refusal is re-filed as local variation, and the validators themselves are discarded once their work completes. Suppression 0.46 is moderate and UNSCALED by context: the mechanism's signature economy is substituting absorption for coercion, so it needs less active force than pure imposition, but sanctions for open defiance and monitoring of conformity remain load-bearing. Theater 0.30: staged-assent rituals grow as the apparatus routinizes, yet validation remains functionally real — it demonstrably stabilizes what imposition alone could not. Accessibility collapse 0.30: understanding the hybrid closes no alternative route; pure imposition and endogenous climbing remain live strategies for other authorities and other episodes. Resistance 0.60: peripheral resistance is endemic — it is the very input the mechanism metabolizes, which is why it stays high across the interval. Fixing cost is prohibitive for the only seat that could fix it: the apex could compensate brokers durably and honor peripheral vetoes, but doing so surrenders the speed and uniformity that motivated the mechanism, so the cost of fixing exceeds the benefit to the fixer. The measurement series run on one shared time grid (every tracked metric authored at every examined point); the falling suppression_requirement series is authored deliberately because the story's traced dynamic IS enforcement substitution — coercion capacity decays as absorption machinery matures.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from the same structure. From the apex seat the arrangement is the solution to its founding problem: durable, roughly uniform adherence without garrisons — coordination it built and owns. From the broker seat it is an opportunity that curdles: standing gained through mediation converts into dependence on continued mediation, then into discard once stabilization removes the need for mediators — the same agent computes as collector early and target late. From the peripheral seat it is obligation imposed and dissent re-labeled: nothing in the structure ever presents a cost to the center, so it reads as one-directional taking throughout. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Apex reformers: declared beneficiary and agenda-setter, institutional power, politically committed exit — d sits near the beneficiary end; they capture the stabilized commitments and certify which local readings count. Mid-level administrators: declared beneficiaries whose returns scale with reported uptake — low d, mildly corrected by their own exposure to hierarchical pressure. Peripheral communities: declared victims, powerless, trapped — d near the full-target end; the binding follows them and their refusals are reclassified rather than answered. Rival tradition holders: declared victims, organized but excluded from the interpretation conversation — high d; their loss is compensated nowhere. Fringe cultural brokers carry a directionality override (moderate atom, d 0.35): the automatic derivation from their beneficiary declaration alone would place them near the subsidized end (~0.15), but their position is genuinely dual — they collect standing while surrendering interpretive autonomy, bearing the adaptation labor, and facing structural discard after stabilization — so the derived d understates their target-side exposure. The override applies to this story's single moderate-power seat, which is uniquely the brokers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the cascade as pure coordination misses the appropriation: validation labor is collected and booked as assent, resistance is absorbed rather than honored, and the validators are discarded — asymmetries a rope framing would erase. Reading it as pure taking misses the coordination: the two-phase structure demonstrably achieves adoption that pure imposition could not at any enforceable cost, and brokers and communities receive real if bounded returns. The mandatrophy watch is forward-looking: as stabilization completes episode by episode, the validation apparatus risks drifting toward inertial maintenance — consultation rituals persisting after the founding problem closes, administrator certification continuing because budgets depend on it, theater_ratio climbing (0.10 to 0.30 across the interval) while the founding problem's status turns contested. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) encodes exactly this split: the original episodes' problem is closed, but the apparatus persists as the standard template for new installations, so the world still rearranges around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_assignment_underdetermination,
    'Which adoption episodes instantiate the hybrid cascade rather than pure endogenous climb or pure exogenous imposition — and is the hybrid a distinct mechanism or a compound of its two siblings?',
    'Code initiation locus and validation locus independently across a case corpus; if episodes sort cleanly into single-mode patterns, the hybrid collapses into one sibling and this story''s epsilon transfers to that sibling''s file.',
    'If the hybrid is exogenous imposition with incidental local noise, epsilon drops toward the imposition reading''s value and fringe validation is theater; if it is sponsored endogenous climb, the two-phase structure is descriptive gloss and the apex''s contribution is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_assignment_underdetermination, conceptual, 'Whether the hybrid cascade is a distinct mechanism or a relabeling of episodes belonging to the sibling readings.').

omega_variable(
    validation_authenticity_ambiguity,
    'When fringe actors validate an installed commitment, are they exercising interpretive authority or performing assent under observation?',
    'Compare durability and content retention of commitments validated without administrator certification against those validated under it; systematic divergence between the two conditions indicates staged assent.',
    'If validation is largely staged, the theater share is understated and the arrangement operates closer to enforced conformity than negotiated adoption; if genuine, the two-phase coordination reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_authenticity_ambiguity, empirical, 'Whether recorded fringe validation reflects exercised authority or observed performance.').

omega_variable(
    absorption_substance_trajectory,
    'Does resistance absorbed as local interpretation retain substantive content across generations, or do locally interpreted variants converge back to the apex form as enforcement deepens?',
    'Trace variant doctrine and practice over three or more generations in locales with differing enforcement intensity; convergence under intensified enforcement marks absorption, persistent divergence marks accommodation.',
    'Convergence confirms the fringe''s interpretive labor is appropriated and then discarded, supporting the asymmetric reading; persistent divergence supports a reciprocal-exchange reading with materially lower net extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_substance_trajectory, empirical, 'Whether absorbed resistance keeps its substance or is diluted into the apex form.').

omega_variable(
    broker_discard_generality,
    'Is post-stabilization marginalization of validating brokers a general structural feature of the cascade or an artifact of particular cases?',
    'Longitudinal comparison of broker cohorts before and after stabilization across multiple installation episodes, controlling for case-specific politics.',
    'If general, the mechanism systematically collects validation labor and then discards the collectors, raising net extraction and sharpening the asymmetry that separates this from a balanced exchange; if case-specific, broker costs are contingent and the exchange sits closer to balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_discard_generality, empirical, 'Whether validator discard is structural or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t4, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(stat_tr_t12, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t4, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(stat_be_t12, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t4, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(stat_su_t12, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 24, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how new state commitments gain legitimacy' covers three structurally distinct claims differing on initiation locus and validation necessity. Per the epsilon-invariance principle the kernel decomposes into a three-story family: endogenous_climb_reading (fringe-originated legitimacy), exogenous_imposition_reading (apex installation sufficient), and this hybrid_cascade_reading (apex-initiated, fringe-validated). Each member carries its own epsilon, beneficiary set, and victims; this story links to both siblings. Epsilon differs across the family because the referent differs: the hybrid reading assesses the two-phase arrangement itself, where validation labor is appropriated and resistance absorbed — a structure neither sibling contains. Evidence disputes flow upstream from both siblings into this one, since any fringe activity the endogenous reading cites as origination can be re-coded here as phase-two validation, and any stabilization the imposition reading cites as installation-success can be re-coded here as incomplete without validation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
