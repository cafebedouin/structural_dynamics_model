% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Meiji Norm Codification as Ratified Bottom-Up Adoption (Endogenous Climb Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   Between 1868 and 1890 the Meiji state codified a cluster of new norms —
 *   official Western dress (1872), the Gregorian calendar (1873), mandatory
 *   commoner surnames in the civil registers (1875), standardized official
 *   timekeeping. This story instantiates the endogenous climb reading of how
 *   those norms gained legitimacy: adoption ran ahead of the edicts, carried
 *   by treaty-port commerce, urban status competition, and administrative
 *   necessity, so that each mandate ratified a convergence already underway
 *   rather than producing compliance through coercion. The constraint under
 *   classification is the codification apparatus itself — the standing
 *   arrangement of edicts, registers, and promulgation — assessed by this
 *   reading's own lights: a coordination instrument whose warrant is prior
 *   popular acceptance. On that assessment the apparatus extracts little (ε
 *   0.18), suppresses little (0.09), and meets little resistance (0.12); the
 *   state's role is coordinator and ratifier. This file is one member of a
 *   three-story constraint family decomposing the imposition-mechanism
 *   question per ε-invariance; the sibling files carry the other readings and
 *   are linked through network.affects_constraints. The claim (rope) and the
 *   metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - meiji_fiscal_authorities: agenda-setter and beneficiary (institutional / arbitrage) — timed each edict to follow visible adoption; collected legibility, fiscal-calendar alignment, and conscription capacity at thin enforcement cost
 *   - treaty_port_merchants: primary beneficiary (powerful / arbitrage) — adopted the new conventions before any mandate to cut foreign-transaction friction; the edicts extended their paid-for standard inland
 *   - adopting_urban_households: beneficiary (moderate / constrained) — took up dress, naming, and time practices for credit, employment, and standing before codification; gained official prestige for an existing practice
 *   - laggard_rural_households: residual cost-bearer outside the ratification narrative (moderate / trapped) — bore registration deadlines, calendar disruption, and formalities; their dissent had no standing under a mandate justified as recording pre-existing consensus
 *   - historical_sociologists: analytical observer — date adoption curves against edict dates; the temporal ordering is the family's central contested measurement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.09).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.09).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Meiji Norm Codification as Ratified Bottom-Up Adoption (Endogenous Climb Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '2d27b495-20b7-4571-bfa2-9b13a5e1b324').
narrative_ontology:cs_kernel_codification('2d27b495-20b7-4571-bfa2-9b13a5e1b324', distributed).
narrative_ontology:cs_authority_grounding('2d27b495-20b7-4571-bfa2-9b13a5e1b324', expertise).
narrative_ontology:cs_interpretation_layer_present('2d27b495-20b7-4571-bfa2-9b13a5e1b324').
narrative_ontology:cs_reading_relation('2d27b495-20b7-4571-bfa2-9b13a5e1b324', imposition_mechanism_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2d27b495-20b7-4571-bfa2-9b13a5e1b324', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('2d27b495-20b7-4571-bfa2-9b13a5e1b324', foundational, legitimacy_precedes_codification).
narrative_ontology:cs_axiom_status(legitimacy_precedes_codification, holdable).
narrative_ontology:cs_axiom_grounding('2d27b495-20b7-4571-bfa2-9b13a5e1b324', legitimacy_precedes_codification, empirically_contingent).
narrative_ontology:cs_axiom('2d27b495-20b7-4571-bfa2-9b13a5e1b324', secondary, state_role_is_ratification_not_coercion).
narrative_ontology:cs_axiom_status(state_role_is_ratification_not_coercion, holdable).
narrative_ontology:cs_axiom_grounding('2d27b495-20b7-4571-bfa2-9b13a5e1b324', state_role_is_ratification_not_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('2d27b495-20b7-4571-bfa2-9b13a5e1b324', popular_adoption_precedence).
narrative_ontology:cs_drift_state('2d27b495-20b7-4571-bfa2-9b13a5e1b324', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2d27b495-20b7-4571-bfa2-9b13a5e1b324', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, meiji_fiscal_authorities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, treaty_port_merchants).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, adopting_urban_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, laggard_rural_households).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, endogenous_norm_diffusion_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the codifying edicts — official Western dress (1872), the Gregorian calendar (1873), mandatory commoner surnames in the civil registers (1875) — timing each to follow adoption already visible in treaty-port commerce and urban practice. Gained a legible population: uniform names for civil registration, a fiscal calendar aligned with treaty obligations, conscription rolls keyed to the registers. Enforcement machinery stayed thin because compliance was largely voluntary before the edicts; the state's main costs were promulgation and registry administration, and its main gain was administrative capacity.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, meiji_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, meiji_fiscal_authorities, beneficiary).

% Adopted Western dress for negotiations, solar-calendar bookkeeping, and standardized timekeeping before any mandate, because misaligned calendars and unfamiliar presentation taxed every foreign transaction. The edicts ratified their practice and extended the standard inland, so suppliers and correspondents converged on conventions the merchants had already paid to adopt. Exit was real throughout: foreign conventions were available to them at any time, which is why their adoption signals demand rather than obedience.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, treaty_port_merchants, beneficiary,
    powerful, biographical, arbitrage, regional).

% Urban households and ambitious commoners took up the new dress, naming, and time practices for credit, employment, and standing before the edicts made them official. Codification standardized what they already did and attached official prestige to it; their main costs were wardrobe and registration formalities. Leaving the standard after codification would mean exiting official recognition entirely, so a pre-existing choice hardened into a default.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, adopting_urban_households, beneficiary,
    moderate, biographical, constrained, national).

% Households that had not taken up the new norms by the edict dates — mostly rural — absorbed the codification's residual costs: registration deadlines and surname formalization under the 1875 mandate, dislocation of festival and agricultural timekeeping off the lunar calendar, and presentation costs where official contact required it. The edicts were justified as recording what 'the people' had already accepted, which left their dissent without standing: a household that had not adopted was by definition outside the consensus the mandate claimed to record. Petitions survive in local archives but shaped no edict, and registration tied them to the standard where they lived.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, laggard_rural_households, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, laggard_rural_households, excluded).

% Date adoption against codification: household registers, merchant ledgers, dress statistics, and local petitions set against edict dates. The temporal ordering is their central evidentiary question, and it is contested — which is precisely why this arrangement is one reading of a contested kernel rather than a settled fact.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, meiji_fiscal_authorities).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The edict apparatus solved a legibility and alignment problem: uniform surnames made civil registration, taxation, and conscription administrable; the solar calendar aligned fiscal and treaty timekeeping with the states Japan had signed with; standardized official dress and presentation made diplomatic recognition routine. It standardized conventions across a population that had already begun converging on them through commerce.
% TRANSFER_FUNCTION: Moves administrative legibility upward to the state — registration capacity, fiscal-calendar alignment, conscription rolls — at near-zero enforcement cost; moves official recognition and prestige downward to adopters; and moves the residual cost of standardization (deadlines, calendar disruption, formalities) onto households that had not yet adopted. Legitimacy itself flows upward: popular practice is the warrant the mandate cites.
% ABSENT_VOICES: Laggard rural households — the people the ratification narrative definitionally excludes, since a mandate justified as recording pre-existing consensus has no seat for those who had not consented. Their objections survive as local petitions and calendar-disorder complaints but entered no edict deliberation. Customary-calendar communities and foreign observers also held views that the 'national acceptance' framing aggregated away.
% DISAPPEARANCE_RATIONALE: The climbed norms themselves would persist — that is this reading's claim — but the codified substrate would not: civil registration keyed to surnames, the fiscal calendar, conscription rolls, and treaty timekeeping were built on the edicts and would need re-coordination from scratch. Administration and commerce would fragment at the margins until a replacement standardization settlement, which is the signature of a live coordination function rather than an inert formality.
% FOUNDING_PROBLEM: A fragmented customary order could not support a modern fiscal-military state: taxation, conscription, treaty obligations, and diplomatic recognition all required a legible population — uniform names, aligned calendars, recognizable official presentation.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal and administrative historians outside the state-benefit set attest the dependency: the 1873 land-tax reform and the conscription ordinance presupposed surname registration and calendar alignment, and foreign diplomatic correspondence of the 1870s independently attests the treaty-clock problem. No source outside the benefiting parties, however, corroborates the stronger claim that acceptance uniformly preceded every edict — the dating dispute is exactly what the sibling readings contest, and this corroboration covers the founding problem, not the reading's precedence axiom.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18 at interval end) because the edicts codified practices much of the governed population had already adopted at its own cost; what extraction remains is the administrative substrate built on the codified norms — surname registration feeding the land tax and conscription — plus the residual compliance costs of households that had not yet adopted. Suppression is minimal (0.09) and its measured requirement declines across the interval (0.13 → 0.09) because voluntary convergence made enforcement machinery progressively unnecessary; that trajectory is the enforcement-decay signature this reading predicts, and the series is authored on one shared six-point grid with the other metrics per the alignment rule. Theater is low (0.16) but nonzero and slowly rising: promulgation framed the state as the originator of changes it actually followed — a mild origin-myth theater that grows as the modernization narrative consolidates — while the codification, registry, and alignment work remains functional. Accessibility collapse is moderate (0.55): official and commercial alternatives to the standardized norms collapsed, but private and customary alternatives (lunar festival calendars, local naming customs) persisted for decades. Resistance is low (0.12): scattered petitions and calendar-disorder complaints, no organized opposition. Suppression here is a raw structural property, unscaled; only extractiveness is scaled by the engine, via directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the residual cost-bearer seat should compute differently. From the merchant and urban-household seats the apparatus is recognition: it standardizes a practice they chose and pays them back in prestige and reduced friction, so the same edicts that bind them were anticipated by their own behavior. From the laggard seat the apparatus is a deadline with costs attached and no seat at the table — the ratification narrative that legitimates the edicts for everyone else is exactly what silences them. The agenda-setter seat experiences the constraint as its own achievement, which is the origin-myth the theater metric partially registers. The engine computes these per-seat classifications from the structural data; this story's rope claim is a story-level claim and does not adjudicate the laggard seat, which the laggard_cost_materiality omega flags.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the merchant, urban-household, and state seats. The merchants' arbitrage-grade exit (foreign conventions were always available) places them nearest the beneficiary end, and their pre-edict adoption is the strongest evidence that the arrangement subsidizes rather than extracts from them. Adopting households are constrained post-codification but aligned with the standard, so their d stays low. The state seat both administers and collects — its gains are legibility and registration capacity — so it sits low-d with concentrated receipt of gains, which is why gain_flow names this seat. Laggard rural households bear the residual costs and are trapped by registration geography, so the derivation places them near the full-target end despite their absence from the victims array; that gap between structural position and declared victim status is deliberate under this reading and is routed to the laggard_cost_materiality omega rather than resolved by declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making a fragmented customary order legible to a fiscal-military state — remains live: the codified norms are still the substrate of civil registration, legal names, fiscal timekeeping, and treaty alignment, so the arrangement has not outlived its function and no mandatrophy is declared. The classification work this reading does is boundary-keeping between coordination and extraction: the same edicts, read through the exogenous sibling, are coercion producing compliance; read through this reading, they are ratified coordination. Both files stand in the corpus with their own ε, and the divergence between their computed types is the measurement the family exists to take. The failure mode this reading must guard against is the reverse drift: if the codification layer ever became purely ceremonial — norms self-sustaining, registries migrated to systems that no longer need the edicts — the mandate would persist as performance, and the theater_ratio series is the early-warning instrument for that transition. If the laggard costs prove material, the honest recomputation is toward a hybrid coordination/extraction profile, not toward this reading's clean coordination verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the imposition_mechanism_kernel does the historical record actually support for the Meiji norm codification — endogenous climb, exogenous override, or hybrid legitimation — and is the endogenous instantiation the right one for this corpus entry?',
    'Comparative classification of the sibling constraint files against the same adoption-curve evidence: household registers, merchant ledgers, dress statistics, and diplomatic records dated against the 1872-1875 edicts.',
    'Under the exogenous reading the same apparatus re-authors with high ε and high suppression and computes as a snare; under the hybrid reading it computes as a tangled_rope with a symbolic-transfer coordination layer. This file''s rope verdict is conditional on the endogenous instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, empirical, 'Kernel-level reading indexicality: which legitimacy mechanism the record supports for the same edicts.').

omega_variable(
    adoption_precedence_dating,
    'Did popular adoption actually precede each edict in each norm domain — dress, calendar, surnames, timekeeping — or does precedence hold only for some domains?',
    'Per-domain adoption-curve reconstruction from registers, ledgers, and local records with edict dates as cut points; domains that fail the precedence test split into their own constraint stories with their own ε.',
    'If precedence fails for surnames or the calendar, the family decomposes further: those domains re-author as exogenous or hybrid constraints, and this story''s ε applies only to the domains where the climb is documented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_precedence_dating, empirical, 'Temporal ordering of adoption versus edict, per norm domain — the located disagreement between the sibling readings.').

omega_variable(
    laggard_cost_materiality,
    'Were the residual costs borne by households that had not adopted at the edict dates — registration deadlines, calendar disruption, dress and formality outlays — material enough to constitute a victim class?',
    'Local petition records, village accounts, and compliance-cost estimates for the 1873-1876 registration and calendar transition.',
    'If material, victims[] must be declared and the constraint recomputes toward a hybrid coordination/extraction profile (coordination for adopters, extraction from laggards); if immaterial, the rope verdict stands with the laggard seat as a marginal residual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(laggard_cost_materiality, empirical, 'Whether the ratification narrative''s silent residual — non-adopters — is a real cost-bearing class.').

omega_variable(
    mandate_function_ambiguity,
    'Is the codification layer functionally necessary to the registration, fiscal, and treaty apparatus it anchors, or is it theatrical ratification of a convergence that would have completed anyway?',
    'Counterfactual and comparative analysis: did registration, taxation, and treaty timekeeping require the edicts, or did equivalent standardizations proceed without them in comparable state-formation transitions?',
    'If theatrical, theater_ratio rises toward the Goodhart threshold, the mandate layer drifts piton-ward, and the state''s coordinator role collapses into ceremony; if functional, the rope verdict holds and the apparatus is load-bearing coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_function_ambiguity, conceptual, 'Functional codification versus theatrical ratification of the mandate layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 1868, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1868, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement_basis(impo_tr_t1868, observed).
narrative_ontology:measurement(impo_tr_t1871, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1871, 0.11).
narrative_ontology:measurement_basis(impo_tr_t1871, observed).
narrative_ontology:measurement(impo_tr_t1873, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1873, 0.13).
narrative_ontology:measurement_basis(impo_tr_t1873, observed).
narrative_ontology:measurement(impo_tr_t1876, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1876, 0.14).
narrative_ontology:measurement_basis(impo_tr_t1876, observed).
narrative_ontology:measurement(impo_tr_t1883, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1883, 0.15).
narrative_ontology:measurement_basis(impo_tr_t1883, observed).
narrative_ontology:measurement(impo_tr_t1890, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 1890, 0.16).
narrative_ontology:measurement_basis(impo_tr_t1890, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t1868, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement_basis(impo_be_t1868, observed).
narrative_ontology:measurement(impo_be_t1871, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1871, 0.13).
narrative_ontology:measurement_basis(impo_be_t1871, observed).
narrative_ontology:measurement(impo_be_t1873, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(impo_be_t1873, observed).
narrative_ontology:measurement(impo_be_t1876, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1876, 0.16).
narrative_ontology:measurement_basis(impo_be_t1876, observed).
narrative_ontology:measurement(impo_be_t1883, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1883, 0.17).
narrative_ontology:measurement_basis(impo_be_t1883, observed).
narrative_ontology:measurement(impo_be_t1890, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 1890, 0.18).
narrative_ontology:measurement_basis(impo_be_t1890, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1868, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1868, 0.13).
narrative_ontology:measurement_basis(impo_su_t1868, observed).
narrative_ontology:measurement(impo_su_t1871, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1871, 0.12).
narrative_ontology:measurement_basis(impo_su_t1871, observed).
narrative_ontology:measurement(impo_su_t1873, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1873, 0.11).
narrative_ontology:measurement_basis(impo_su_t1873, observed).
narrative_ontology:measurement(impo_su_t1876, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1876, 0.11).
narrative_ontology:measurement_basis(impo_su_t1876, observed).
narrative_ontology:measurement(impo_su_t1883, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1883, 0.1).
narrative_ontology:measurement_basis(impo_su_t1883, observed).
narrative_ontology:measurement(impo_su_t1890, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 1890, 0.09).
narrative_ontology:measurement_basis(impo_su_t1890, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'how did the new Meiji norms gain legitimacy?' conflates three structurally distinct claims about the same edict apparatus. Per the ε-invariance principle it decomposes into a three-story constraint family: this file (endogenous_climb_reading — legitimacy precedes codification, low ε, rope claim), imposition_mechanism_kernel__exogenous_override_reading (coercion produces legitimacy, high ε, snare-flavored), and imposition_mechanism_kernel__hybrid_legitimation_reading (symbolic authority transfer plus institutional incentives, hybrid profile). Each story authors its own ε over the same standing arrangement; the upstream endogenous claim (adoption curves preceding edicts) is cited as evidence by the hybrid reading and contested by the exogenous reading, so the edges run from this story to both siblings. Where the readings disagree is one structural element: the temporal ordering of popular adoption versus edict, per norm domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
