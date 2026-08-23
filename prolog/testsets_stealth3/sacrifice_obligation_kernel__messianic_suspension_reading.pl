% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension of the Sacrificial Obligation (Readiness-Maintenance Reading)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   After 70 CE a covenantally mandatory sacrificial order lost its venue,
 *   personnel purity, and apparatus. This file instantiates ONE reading of
 *   that standing arrangement: the obligation is divinely suspended — neither
 *   violated nor transformed — until messianic restoration, and study of the
 *   sacrificial laws maintains operational readiness for that restoration
 *   rather than fulfilling anything. Under this reading the arrangement has a
 *   genuine coordination function (preserving perishable operational
 *   knowledge across indefinite dormancy), participants are net beneficiaries
 *   within their own valuation (covenantal continuity), and no victim set
 *   exists during suspension: non-performance carries no liability because
 *   the obligation itself is lifted. The ε referent is the standing
 *   suspension-plus-study arrangement as this reading assesses it — not the
 *   restoration-state arrangement it anticipates, and not any sibling
 *   reading's instantiation. KEY AGENTS (by structural relationship): see
 *   key_agents; the future beneficiary cohort is authored as a non-agent
 *   seat.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: agenda-setting seat (institutional / identity_locked) — administers the suspension ruling, fixes curriculum, licenses preparation while prohibiting performance
 *   - priestly_lineages: dual-positioned beneficiary/payer (organized / identity_locked) — maintains eligibility records and bears readiness costs against deferred benefit
 *   - torah_scholars: primary cost-bearing seat (moderate / constrained) — studies the deferred corpus, supported by communal funds
 *   - observant_communities: net beneficiary with diffuse funding costs (organized / constrained)
 *   - temple_readiness_movements: preparation-intensive faction (organized / identity_locked) — pushes the restoration timeline past what authorities license
 *   - site_custodial_authorities: excluded external controller of venue access (powerful / mobile) — coincidental enforcement ally outside the authorization frame
 *   - secular_descendants_of_priestly_lines: excluded enrolled population (moderate / mobile) — tracked genealogically without consent
 *   - future_messianic_generation: non-agent beneficiary seat (powerless / trapped) — the cohort preserved capacity is held for
 *   - comparative_religion_analysts: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.32).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension of the Sacrificial Obligation (Readiness-Maintenance Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'c2948dbb-0e5f-4943-b2a2-6db0b2dad88f').
narrative_ontology:cs_kernel_codification('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', fixed_text).
narrative_ontology:cs_authority_grounding('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', lineage).
narrative_ontology:cs_interpretation_layer_present('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f').
narrative_ontology:cs_reading_relation('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', foundational, suspension_not_abrogation).
narrative_ontology:cs_axiom_status(suspension_not_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', suspension_not_abrogation, theological).
narrative_ontology:cs_axiom('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', foundational, study_preserves_operational_capacity).
narrative_ontology:cs_axiom_status(study_preserves_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', study_preserves_operational_capacity, instrumental).
narrative_ontology:cs_axiom('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', secondary, unauthorized_performance_prohibited_pending_restoration).
narrative_ontology:cs_axiom_status(unauthorized_performance_prohibited_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', unauthorized_performance_prohibited_pending_restoration, conventional).
narrative_ontology:cs_reference_frame('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', divinely_legislated_dormant_order).
narrative_ontology:cs_drift_state('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c2948dbb-0e5f-4943-b2a2-6db0b2dad88f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_messianic_generation).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, observant_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, torah_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, temple_readiness_movements).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, torah_scholars).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, observant_communities).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, temple_readiness_movements).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, continuity_of_covenantal_obligation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, oral_torah_transmission_reliability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codify, transmit, and adjudicate the ruling that the sacrificial order lies dormant pending restoration: they fix the study curriculum, rule on priestly qualification and ritual detail, and issue prohibitions when groups attempt performance ahead of restoration. Their standing rests on stewardship of the transmitted corpus; stepping outside the tradition's frame would dissolve the office they hold. They collect deference and institutional support and bear the responsibility of keeping the dormant order coherent across generations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Families of hereditary priestly descent maintain genealogical records establishing eligibility for future temple service, often preserve endogamous marriage norms, and many study the temple-service tractates in depth. They carry present readiness costs — education, record-keeping, restricted marriage pools — against a benefit scheduled for a restoration they may not live to see. Letting lineage registration lapse would mean abandoning an inheritance rather than exercising a choice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages, payer).

% Devote years of study to the sacrificial-law corpus whose practical application is indefinitely deferred; they receive stipends, teaching posts, and communal honor funded by the wider community, and produce the commentaries and trained teachers through which the material survives. Redirecting to other fields is possible and common at the margins, but deep specialization carries sunk investment and reputational anchoring within the academy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, torah_scholars, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, torah_scholars, beneficiary).

% Fund the academies, recite the thrice-daily petition for restoration, and organize communal life around the anticipation that the dormant order will reactivate; they receive continuity of covenantal practice and communal identity and bear the indirect cost of supporting a large specialized study sector. Leaving the observant fold carries real social and familial cost, though exit is legally possible.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, observant_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, observant_communities, payer).

% Reconstruct vessels, garments, and architectural plans, train candidates for priestly service, and press for conditions approaching performance sooner rather than later; they spend heavily on preparation and draw purpose and donations from the prospect of restoration. Established halakhic authorities license much of their preparation while prohibiting the performance steps they most want to take, leaving them in permanent tension with the offices they appeal to.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, temple_readiness_movements, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, temple_readiness_movements, beneficiary).

% Control physical access to the Temple Mount under security and diplomatic arrangements that have no halakhic content; their enforcement of the visitation status quo incidentally blocks any performance attempt. They are not party to the tradition's internal authorization and would oppose changes at the site originating from any quarter.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, site_custodial_authorities, excluded,
    powerful, biographical, mobile, regional).

% Carry kohanic ancestry without observance; the readiness apparatus tracks their lineage status through registries and marriage norms they did not opt into, and they would object that a restoration project is being provisioned on their behalf, and at their genealogical expense, without their consent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, secular_descendants_of_priestly_lines, excluded,
    moderate, biographical, mobile, global).

% Does not yet exist; inherits whatever capacity, obligations, and expectations the present regime preserves or forecloses. Cannot consent, decline, or negotiate the terms it will live under; listed for completeness as the cohort the preserved capacity is held for.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_messianic_generation, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__messianic_suspension_reading, future_messianic_generation).

% Document how post-70 CE communities converted a venue-bound cultus into a text-and-curriculum regime; they sit outside the normative dispute and observe the full structure of competing readings defining what the obligation now is.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, comparative_religion_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, torah_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complete operational apparatus of a temple-dependent cultus — legal rulings, priestly genealogies, procedural detail, qualifying personnel — across indefinite dormancy without practice opportunities, so that performance can resume immediately upon restoration; simultaneously holds the line against unauthorized performance attempts that purity and site constraints forbid.
% TRANSFER_FUNCTION: Moves time, attention, and funding from the living community — students, patrons, laity — into maintenance of a dormant capacity: a stock of transmitted knowledge, registered lineages, and prepared instruments held for a future generation.
% ABSENT_VOICES: Site custodial authorities control the physical venue yet stand wholly outside the halakhic conversation; secular descendants of priestly lines are enrolled genealogically without consent and would object to provisioning done on their behalf; Jews who have left the tradition would object that readiness-maintenance presumes a restoration they reject; within the tradition, voices pressing immediate performance are marginalized as fringe by the offices they challenge.
% DISAPPEARANCE_RATIONALE: If the suspension-plus-study arrangement vanished overnight, the community would face an unresolvable fork: attempt forbidden performance (confrontation at the site, schism with the authorities, purity impossibilities) or formally abrogate the obligation (rupture with the covenantal frame that organizes liturgy, curriculum, and lineage law). Daily liturgy, academy curricula, priestly registries, and endogamy norms would all restructure within a generation.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) left a covenantally mandated sacrificial system without its required venue, personnel purity conditions, or apparatus; the community needed a ruling on whether the obligation had lapsed, was being violated daily, or lay dormant awaiting restoration.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the thrice-daily liturgical petition for restoration predates and grounds the study regime and is attested uniformly across all major liturgical rites; medieval responsa literature and public disputations repeatedly adjudicate the resumed-performance question; modern academic historians of religion independently document the post-70 CE adjudication as the community's central crisis. Even the rival readings concede the founding problem exists — what they dispute is its resolution, which is itself evidence the problem is not a beneficiary-authored myth.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18): the arrangement's costs — years of deferred-corpus study, academy funding, endogamy restrictions — are accepted within the community as covenantal service, no liability attaches during suspension, and the benefits (continuity, identity, preserved capacity) accrue under the participants' own valuation. Suppression (0.32) reflects the closed performance-exit: halakhic prohibition plus physical site closure block anyone who would sacrifice now, while participation in study itself is lightly compelled and largely internalized — the suppression is mostly internalized acceptance layered over a structural site closure, not active coercion of participants. Theater ratio (0.12) is low because study output is load-bearing: registries, trained teachers, and responsa genuinely transmit capacity; a commemorative and museum layer grows at the margin (vessel exhibitions, touristic presentation), which is why the series ends slightly above its historical floor. Accessibility_collapse (0.45): once the reading is adopted, performance-now and formal-abandonment both close within the framework, but three rival readings remain institutionally live across communities, so alternatives collapse only partially. Resistance (0.22): episodic activist performance attempts and modernizing defection, otherwise broad compliance. The claimed_type (rope) is authored independently from these metrics: the arrangement solves a real collective-action problem (dormant-capacity preservation) with minimal coercive overhead and no victim set — the engine computes per-seat classifications and any divergence from the claim is the datum. All three tracked series run on one shared time grid (70, 250, 600, 1000, 1400, 1750, 1900, 2025) with every metric authored at every point; suppression_requirement is tracked because the story specifically traces enforcement-capacity change (crisis-era vigilance, long quiescence while external reality blocked the site, renewed enforcement demand as activist pressure returns), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats inside one normative frame should compute differently. The agenda-setter experiences the arrangement as faithful stewardship of a divine decree — near-zero burden, high meaning. The scholar experiences deferral cost against a personally uncertain payoff. The readiness movements experience the licensed/prohibited line as intolerable delay imposed by the very offices they fund and venerate. The secular enrolled descendants experience registration without representation. The site custodians, holding physical power over the venue, experience the entire dispute as a security file. Same framework, four lived realities; the engine derives this divergence from power, exit, and role data — the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: observant_communities and priestly_lineages sit near the beneficiary end (continuity and deferred eligibility outweigh diffuse costs); halakhic_authorities sit near the beneficiary end as administrators whose standing the arrangement confers. The future_messianic_generation is authored as a non-agent and is excluded from derivation — a not-yet-existing cohort must not feed d-to-chi as if it collected now. torah_scholars are the exception requiring an override: the pure-beneficiary derivation from the beneficiaries array alone would place them near d~0.1, understating that they bear the arrangement's most concentrated present cost (years of deferred-application study); the override sets moderate-power agents to d=0.45, near-symmetric, reflecting cost-bearing offset by support, standing, and role meaning. Site custodial authorities sit outside the authorization frame entirely — their enforcement of non-performance is coincidental alignment, not a structural party relationship, and no override is authored for them. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness passes through directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a venue-less obligation — remains live under this reading: every generation re-inherits it through the liturgy itself, so the (founding_problem_status=live x disappearance_verdict=world_rearranges) cell is matched and no dead-mandate zombie flag is expected. The risk pathway this reading must guard against is different: the arrangement's justification is transitional (valid until restoration) yet carries no declared sunset — the terminating condition is an undated eschatological event. If restoration is indefinitely deferred, the transition-justification erodes, curriculum composition drifts commemorative, and the arrangement would slide toward archive-function with residual readiness rhetoric — the inertial signature — while the readiness claim persists verbally. Keeping study instrumentally defined (capacity maintenance, not fulfillment) is what makes that atrophy detectable as a rising theater_ratio rather than hiding it behind substitutive-success language; this is also the precise structural fault line with the study_as_exercise sibling, which would convert deferred cost into present fulfillment and dissolve the obsolescence question altogether.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Which of the four readings of sacrifice_obligation_kernel governs the arrangement being classified — this file instantiates only the messianic_suspension_reading, and the sibling readings instantiate structurally different arrangements over the same legislation?',
    'Comparative institutional observation across communities: which curriculum justification predominates, whether study is framed as discharge or as preparation, whether performance attempts are treated as liability or as category error.',
    'Under the substitutive sibling, present practitioners join the beneficiary set and the deferred-cost structure disappears; under performance_only, a liability victim set appears and extraction rises sharply; under the archive sibling, halakhic extraction vanishes entirely and the type space collapses toward inertial categories. Classification is indexical to the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'Committer-frame omega recording that this story is one reading of a four-way kernel partition; sibling readings are separate constraints.').

omega_variable(
    study_function_instrumental_vs_substitutive,
    'Is study of the sacrificial corpus strictly instrumental (maintaining capacity for a future performance) or does it constitute present exercise of the obligation?',
    'Located disagreement with study_as_exercise_reading: analyze whether the tradition''s own texts treat study as discharging the obligation (substitutive formulae in liturgy and jurisprudence) or as preparation only; the two premises cannot coexist in one framework, so the dispute resolves only by which framework a community adopts.',
    'If substitutive, this reading''s low epsilon is misattributed — the arrangement''s cost is fulfillment, beneficiaries include present practitioners, and the readiness-maintenance rationale (and its obsolescence exposure) evaporates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_function_instrumental_vs_substitutive, conceptual, 'The specific structural element on which this reading and its exercise-reading sibling diverge.').

omega_variable(
    restoration_non_arrival_drift,
    'If restoration never arrives, does the arrangement drift from capacity-maintenance toward commemorative archive-function while retaining readiness rhetoric?',
    'Longitudinal curriculum and rhetoric analysis: track the operational-to-commemorative share of korbanot study output, the decay of practical procedure examination, and the growth of exhibition-oriented presentation across successive generations.',
    'Confirmed drift would date a rope-to-inertial transition: the transitional justification decays without a declared sunset, theater_ratio rises, and the readiness claim becomes performance rather than function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_non_arrival_drift, empirical, 'Persistence-without-event trajectory for an undated-transition arrangement.').

omega_variable(
    operational_capacity_reality,
    'Does the study regime actually preserve performable capacity — procedural precision for the temple service, reliable identification of eligible priests — or has capacity already decayed into commemorative familiarity?',
    'Structured examination of advanced students against reconstructed procedure sequences, plus audit of genealogical registry integrity and eligibility determination practices.',
    'If capacity has decayed, the low theater_ratio is overstated, the readiness justification fails on its own instrumental terms, and the arrangement is already functioning as heritage while claiming function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_capacity_reality, empirical, 'Whether the instrumental premise of the readiness regime holds empirically.').

omega_variable(
    nonconsenting_genealogical_enrollment,
    'Does readiness-maintenance impose costs on non-consenting enrolled persons — secular carriers of priestly lineage tracked by registries and endogamy expectations they never affirmed — such that a latent victim set exists during suspension?',
    'Survey and legal analysis of burden incidence on enrolled-but-non-observant lineage carriers: marriage-pool restriction, communal expectation, documentary tracking; and whether any coalition of enrolled persons could plausibly withdraw from the registries.',
    'A demonstrated burden would contradict the no-victim-during-suspension delta, introduce a payer seat the current authoring lacks, and push classification away from pure coordination toward hybrid extraction; a finding of negligible burden confirms the delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonconsenting_genealogical_enrollment, preference, 'Latent victim-set question arising from consent-free enrollment of lineage carriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(messianic_suspension_tr_t70, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 70, 0.08).
narrative_ontology:measurement(messianic_suspension_tr_t250, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 250, 0.05).
narrative_ontology:measurement(messianic_suspension_tr_t600, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 600, 0.06).
narrative_ontology:measurement(messianic_suspension_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(messianic_suspension_tr_t1400, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1400, 0.07).
narrative_ontology:measurement(messianic_suspension_tr_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1750, 0.06).
narrative_ontology:measurement(messianic_suspension_tr_t1900, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(messianic_suspension_tr_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(messianic_suspension_be_t70, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 70, 0.3).
narrative_ontology:measurement(messianic_suspension_be_t250, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 250, 0.26).
narrative_ontology:measurement(messianic_suspension_be_t600, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 600, 0.2).
narrative_ontology:measurement(messianic_suspension_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.17).
narrative_ontology:measurement(messianic_suspension_be_t1400, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1400, 0.16).
narrative_ontology:measurement(messianic_suspension_be_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement(messianic_suspension_be_t1900, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1900, 0.17).
narrative_ontology:measurement(messianic_suspension_be_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(messianic_suspension_su_t70, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 70, 0.36).
narrative_ontology:measurement(messianic_suspension_su_t250, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 250, 0.3).
narrative_ontology:measurement(messianic_suspension_su_t600, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 600, 0.22).
narrative_ontology:measurement(messianic_suspension_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.18).
narrative_ontology:measurement(messianic_suspension_su_t1400, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1400, 0.18).
narrative_ontology:measurement(messianic_suspension_su_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1750, 0.16).
narrative_ontology:measurement(messianic_suspension_su_t1900, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement(messianic_suspension_su_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2025, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: sacrifice_obligation_kernel decomposes into four readings per the epsilon-invariance principle — each reading instantiates a distinct arrangement with its own epsilon, beneficiary structure, and type, so no single story may average across them. This member (messianic_suspension_reading) authors low epsilon over a suspension-plus-readiness arrangement with no present victim set. study_as_exercise_reading authors epsilon over a substitutive arrangement where study-cost is fulfillment and present practitioners join the beneficiary set. performance_only_reading authors epsilon over a pressing-obligation arrangement carrying a liability victim set. symbolic_archive_reading authors epsilon near zero over a heritage arrangement with no halakhic extraction. Upstream/downstream structure: the suspension reading supplies the capacity-preservation rationale that the archive reading secularizes; the performance reading cites the same fixed legislation to deny suspension; the exercise reading absorbs the suspension reading's study-practice while inverting its function. All members are linked via affects_constraints; the located disagreements are documented in each file's omegas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
