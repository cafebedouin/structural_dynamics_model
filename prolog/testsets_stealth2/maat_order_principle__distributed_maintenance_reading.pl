% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Distributed Maintenance of Ma'at (Station-Conduct Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The Ma'at kernel, the claim that cosmic and social order is sustained by
 *   right conduct, admits rival accounts of who maintains the order and who
 *   may interpret the standard. This file instantiates exactly one of those
 *   accounts, the distributed_maintenance_reading: maintenance is everyone's
 *   work from Pharaoh to commoner, each in station; interpretive authority is
 *   plural (temples, courts, households, instruction texts); and authority
 *   attaches to demonstrated maintenance rather than inherent status. The
 *   sibling readings, divine_mandate_reading and reciprocity_reading, are
 *   separate constraint files with their own epsilon values, beneficiary
 *   structures, and classifications; this story neither averages over them
 *   nor hedges against them. The claim/metric independence rule is honored
 *   deliberately: claimed_type is authored as rope because the reading's own
 *   structural logic is broad-based coordination with accountability running
 *   in every direction, while the metrics are authored as the descriptive
 *   record shows, including the residue of station-assigned burden and the
 *   slow compounding of temple endowments that the reading itself would
 *   acknowledge as its imperfection.
 *
 * KEY AGENTS:
 *   - pharaoh: chief visible maintainer (institutional/identity_locked) — performs or delegates the rites, bears the largest named obligation, cannot leave the office
 *   - temple_priesthood: ritual administrator and accumulating collector (institutional/constrained) — interprets the standard in precinct, compounds endowment income
 *   - vizier_judiciary: judicial administrator (institutional/constrained) — wears the standard at the neck while judging, career indexed to fidelity
 *   - scribal_officials: recording agent (moderate/mobile) — assesses and documents the flows, widest internal exit in the system
 *   - peasant_farmers: mass contributor (powerless/trapped) — their labor is the bulk of maintenance; assessment lands on them first
 *   - artisan_workmen: organized contributor (organized/trapped) — ration-dependent specialists who proved the bargain binds upward by striking
 *   - bondservants_captives: excluded seat (powerless/trapped) — station-conduct addressed to them consecrates bondage; outside the record
 *   - wisdom_text_scribes: analytical seat (moderate/analytical) — articulates the standard and transmits its internal critics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.22).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.3).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Distributed Maintenance of Ma'at (Station-Conduct Reading)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'f8f2481c-95ef-4b84-8010-ffd2a62f16df').
narrative_ontology:cs_kernel_codification('f8f2481c-95ef-4b84-8010-ffd2a62f16df', distributed).
narrative_ontology:cs_authority_grounding('f8f2481c-95ef-4b84-8010-ffd2a62f16df', practice).
narrative_ontology:cs_interpretation_layer_present('f8f2481c-95ef-4b84-8010-ffd2a62f16df').
narrative_ontology:cs_reading_relation('f8f2481c-95ef-4b84-8010-ffd2a62f16df', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('f8f2481c-95ef-4b84-8010-ffd2a62f16df', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('f8f2481c-95ef-4b84-8010-ffd2a62f16df', foundational, ma_at_maintenance_universally_distributed).
narrative_ontology:cs_axiom_status(ma_at_maintenance_universally_distributed, holdable).
narrative_ontology:cs_axiom_grounding('f8f2481c-95ef-4b84-8010-ffd2a62f16df', ma_at_maintenance_universally_distributed, deontological).
narrative_ontology:cs_axiom('f8f2481c-95ef-4b84-8010-ffd2a62f16df', foundational, ruler_accountable_to_ma_at_standard).
narrative_ontology:cs_axiom_status(ruler_accountable_to_ma_at_standard, holdable).
narrative_ontology:cs_axiom_grounding('f8f2481c-95ef-4b84-8010-ffd2a62f16df', ruler_accountable_to_ma_at_standard, deontological).
narrative_ontology:cs_axiom('f8f2481c-95ef-4b84-8010-ffd2a62f16df', secondary, station_conduct_sufficiency).
narrative_ontology:cs_axiom_status(station_conduct_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('f8f2481c-95ef-4b84-8010-ffd2a62f16df', station_conduct_sufficiency, conventional).
narrative_ontology:cs_reference_frame('f8f2481c-95ef-4b84-8010-ffd2a62f16df', distributed_station_conduct).
narrative_ontology:cs_drift_state('f8f2481c-95ef-4b84-8010-ffd2a62f16df', ramesside_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f8f2481c-95ef-4b84-8010-ffd2a62f16df', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, peasant_farmers).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, artisan_workmen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, peasant_farmers).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, artisan_workmen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs, or formally delegates, the daily temple rites that present order to the gods; issues the festival calendar, endows temples, and hears cases at festival tribunals. The tax surplus that feeds the whole apparatus is collected in his name. In this telling he is the most visible keeper of the order, not its sole owner: a failed inundation or a corrupt judge counts as his personal lapse, and the instruction literature imagines a king being corrected. Leaving the office is not a step he can take; the office is what he is.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Keeps the daily offering schedule, stages festival processions, consults oracles, and supplies the lector-priests who sit in courts. Collects tithes, first-fruits, and endowment income that compounds into estates which, by the later New Kingdom, rival the crown's. Their word is authoritative inside the temple precincts but competes there with the vizier's judgment and everywhere else with household piety. Departure means losing stipend, station, and burial provision.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary).

% Receives an installation charge commanding judgment by the order-standard, 'so that the crocodile is not preferred over the fish'; hearing judges wear a small figure of the goddess at the neck while cases are argued. They settle disputes, register land, prosecute corrupt officials, and their tomb inscriptions advertise verdicts as career credentials. Advancement and survival run through visible fidelity to the standard they administer.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, vizier_judiciary, agenda_setter,
    institutional, biographical, constrained, national).

% Assess and record the grain, labor days, and litigation the whole system runs on, ideally 'with honest measure.' The instruction genre contrasts their soft hands with the farmer's misery, marking how much the station yields them. Literacy travels: a scribe can move between treasury, temple, and estate administrations, the widest internal mobility the system affords.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_officials, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, scribal_officials, agenda_setter).

% Plow, sow, and reap the fields whose yield the theology reads as order made visible; deliver tax grain, answer corvee summons for canals, tombs, and temples, and attend the festivals that renew the bond. Their conduct is the bulk of what the doctrine calls maintenance, and they receive the ordered world it promises, but the assessment lands on them first, and in a bad year their contribution is subtracted from survival rather than added to abundance.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, peasant_farmers, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, peasant_farmers, beneficiary).

% The royal-tomb crews of the Theban west bank: cut, paint, and furnish eternity for the court while living on state rations in a purpose-built village. When rations fail in Year 29 of Ramesses III they lay down tools and sit at the mortuary temple until grain arrives, the earliest recorded strike, arguing that the provisioning half of the bargain had lapsed. Organized, lettered enough to petition and litigate, but bound to the village and the works.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, artisan_workmen, payer,
    organized, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, artisan_workmen, beneficiary).

% War captives and debt-servants whose assigned place is bondage; the teaching reaches them only as 'keep the conduct of the station you occupy.' They would testify that a doctrine of station-conduct blesses the chain as readily as the plow. They appear in the surviving record chiefly as price lines and property inventories, not as speakers.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, bondservants_captives, excluded,
    powerless, immediate, trapped, local).

% Compose and copy the instruction books, Ptahhotep, Amenemope, the Satire of Trades, that specify what each station's proper conduct is, and praise the order-principle as 'a great thing, its effectiveness everlasting.' The same scribal culture transmits the harpers' songs that mock the efficacy economy and the Dialogue in which a man argues with his own soul; analysis and doubt share a workshop.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, wisdom_text_scribes, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of sustaining social and cosmic order across a riverine civilization without a coercive post at every node: each actor's internalized station-conduct (honest weights, timely plowing, just verdicts, correct ritual, fed neighbors) aggregates into maintained order, on which the inundation, the harvest, and access to the afterlife are all held to depend.
% TRANSFER_FUNCTION: Moves labor, grain, taxes, ritual service, and truthful testimony from every station upward into the maintenance apparatus of temples, courts, and royal works, and moves legitimacy, festival life, juridical protection, and afterlife assurance back down to every station; the nominal exchange is symmetric, the material collection skews toward temple and palace.
% ABSENT_VOICES: Bondservants and war captives, whose assigned station is bondage, would object that station-conduct consecrates their subjugation; provincial poor taxed past subsistence in lean years would object that maintenance rhetoric outbids survival; skeptics of the harpers'-song type would deny the efficacy economy outright. Where they are: almost entirely outside the literate record, which the top few percent of the society authored; their objections survive only obliquely, in satire, in strike transcripts, and in the silences of property lists.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, courts would lose their decision standard (judges literally wore the goddess at the neck), oaths would lose their force, the festival and offering economies would stop, tax and corvee would lose their justifying frame, funerary practice would become incoherent, and the workmen's strike weapon, the appeal to a lapsed bargain, would dissolve. Nearly every institution on the Nile banks would need to renegotiate its warrant.
% FOUNDING_PROBLEM: After the Old Kingdom's collapse dissolved the royal guarantee, how does order survive without a single guarantor? The distributed answer: spread the maintenance burden across every station so that order is produced by aggregate conduct rather than issued from a throne, and make every interpreter of the standard answerable to the standard itself.
% FOUNDING_PROBLEM_CORROBORATION: Royal inscriptions attest the problem from inside the benefiting circle and are discounted accordingly. Corroboration from outside it: the Turin Strike Papyrus, the workmen's own record, shows subordinates invoking the provisioning half of the bargain against the palace and winning concessions, which corroborates that maintenance obligations were understood to bind upward; the tomb-robbery confessions show officials expecting judgment under the same standard they administered; and modern Egyptology (Assmann, Franke) independently reconstructs the democratization trajectory from coffin texts to commoner heart scarabs. Plainly stated: no fully external ancient attestation exists, since neighboring cultures left almost no record of Egyptian ideological debate, and that absence is itself signal about who could write.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because under this reading accountability runs to every station including the top, no seat monopolizes interpretation, and the returns (order, juridical protection, afterlife access) are distributed with the burdens; the residue reflects station-assigned obligations that the occupant did not choose and the compounding material skew toward temple estates. Suppression (0.30) is a raw structural property, unscaled by power or scope: sanction is diffuse (oath before the court, judgment theology, social shame, the negative confession) rather than a dedicated coercive machine, and rival practices such as direct personal piety and skeptical songs persist unpunished. Theater_ratio (0.18) is low because ritual and judgment are functional acts within the worldview, with a modest performative surplus that grows as New Kingdom liturgy elaborates. Accessibility_collapse (0.40) is moderate-low: understanding the standard does not eliminate alternatives, since piety movements route around the temples and the harpers' songs circulate inside the very scribal culture that copies the orthodoxy. Resistance (0.28) is real but voiced in the standard's own terms, the Deir el-Medina strike being the paradigm: workers halted the works citing the lapsed provisioning half of the bargain, which is resistance that confirms the frame it contests. The suppression_requirement series is included because the story genuinely traces enforcement-capacity change: central coercive capacity collapsed at the First Intermediate Period onset while the arrangement persisted on distributed maintenance, then judicial machinery partially rebuilt under the Middle Kingdom and stabilized through the Ramesside era. All three tracked series share one time grid (points 0 through 60 by tens) so no metric row is backfilled from another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the pharaoh's position the arrangement is legitimation inseparable from an obligation he cannot shed; identity_locked exit means he experiences even low extraction as total, since there is no outside. From the temple_priesthood position it is vocation compounded with revenue, and their constrained exit makes the endowment stream feel earned. From the peasant_farmers' position it is cosmic dignity wrapped around a hard exaction, with trapped exit converting bad-year assessments into survival loss. The artisan_workmen seat computes the arrangement as an enforceable contract, which is exactly what the strike demonstrates. The bondservants_captives seat, excluded from the conversation and absent from the victim roll, would compute the highest extraction of all, since station-conduct addressed to the enslaved consecrates the enslavement; the engine sees their trapped exit and powerless atom even though no declaration names them victims. The scribal_officials seat, mobile and moderately rewarded, computes the mildest load. The engine derives these divergences from the declarations and exit atoms; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared broadly (pharaoh, temple_priesthood, scribal_officials, peasant_farmers, artisan_workmen) and no victims are declared, because under this reading's own lights no group is a designated casualty of the arrangement; the burden side is carried instead by the payer roles and the exit atoms. Most seats therefore derive directionality near the beneficiary end. The agenda_setter duties of pharaoh and temple_priesthood pull their derived d slightly up from pure beneficiary, since administering the standard is itself a cost. Trapped exits (peasant_farmers, artisan_workmen, bondservants_captives) push those seats toward the target end despite their beneficiary listings, because the engine weights exit modulation; the scribal_officials' arbitrage-grade internal mobility pulls their d down. No directionality_overrides are authored: the derivation from declarations plus exit atoms already captures the structure, and the one coarse-instrument risk (two institutional seats, pharaoh and temple_priesthood, deriving similar d) is acceptable because both genuinely sit low, differing mainly in accumulation rate rather than direction. On the receipt surface, gain_flow names temple_priesthood: the material flows the arrangement collects (tithes, first-fruits, endowment income) demonstrably accrue there and compound, which is receipt without interpretive monopoly, since authority over the standard remains plural under this reading. fixing_cost is prohibitive: removing the arrangement would mean dismantling the courts, the calendar, the festival economy, the tax warrant, and the funerary system at once, a civilizational rewiring whose cost dwarfs any benefit to the seats positioned to attempt it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, order without a guarantor after the Old Kingdom collapse, is still live, so nothing here has mandated outlived its function and no mandatrophy resolution is declared. The mandatrophy risk sits in the tail of the measurement series: theater_ratio creeps upward across the interval as temple liturgy elaborates and endowments compound, which is the early signature of maintenance drifting toward performance and collection. The classification discipline cuts both ways: reading this as a snare would erase the genuine coordination that the strike record proves (subordinates could and did enforce the bargain upward), while reading it as frictionless rope would erase the station-legitimation residue that the omegas flag, where 'keep the conduct of your station' quietly consecrates inherited inequality. The rope claim plus honest metrics leaves that residue visible to the engine instead of reconciled away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_relative_epsilon,
    'Does the low epsilon authored here belong to the underlying Ma''at arrangement itself, or only to the distributed_maintenance_reading''s construction of it? This constraint is one reading of kernel maat_order_principle; the disagreement among readings is located in the accountability locus, who maintains the order and who may lapse under the standard.',
    'Compile and compare the sibling stories (divine_mandate_reading, reciprocity_reading) over the same historical span: if the same institutions compute as high-extraction under the divine mandate instantiation and low-extraction here, the divergence is reading-indexed rather than arrangement-indexed, which is the expected outcome under the epsilon-referent rule.',
    'If the arrangement itself is better modeled by the divine mandate reading, this file''s low epsilon is an artifact of the reading''s own lights and the effective classification of the lived institutions shifts sharply toward the extractive types; if the distributed reading tracks the operative norm, the low values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relative_epsilon, conceptual, 'Whether authored epsilon is a property of the arrangement or of the reading instantiating it.').

omega_variable(
    station_assignment_legitimacy,
    'Is ''proper conduct in your station'' a neutral division of maintenance labor, or an ideology that consecrates inherited inequality by dressing assigned burdens as cosmic contributions?',
    'Comparative mobility evidence: attested cases of cross-station advancement (peasant sons entering scribal schools, military-era promotions), the Satire of Trades'' own ambivalence about station assignment, and wage/ration data across stations relative to contribution.',
    'If consecration dominates, the effective extraction borne by the lower stations is far higher than the authored 0.22 suggests and the computed classification drifts toward tangled_rope; if the division is genuinely functional, the rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(station_assignment_legitimacy, empirical, 'Whether station-conduct distributes burdens neutrally or launders hierarchy.').

omega_variable(
    democratization_evidence_bias,
    'Did distributed maintenance actually widen downward across the interval (coffin texts to Book of the Dead to commoner heart scarabs), or does the apparent widening reflect preservation bias favoring later, more durable media?',
    'Stratified archaeological comparison of funerary assemblages by class against textual attestation rates per period, controlling for substrate and excavation history.',
    'If the widening is artifact, the reading''s authority-widening claim is aspirational rather than descriptive, the reference_frame drift assessment weakens, and the low suppression value needs revisiting since the plural-interpreter picture would rest on thin evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratization_evidence_bias, empirical, 'Whether the downward spread of maintenance agency is real or a preservation artifact.').

omega_variable(
    bottom_up_enforcement_signal,
    'Do episodes like the Year 29 Deir el-Medina strike show the distributed frame functioning (accountability enforceable from below) or fraying (maintenance breaking down under fiscal stress)?',
    'Frequency, duration, and outcome coding of labor actions, petition letters, and oracle appeals across the Ramesside administrative archives, scored for whether invocation of the standard produced redress.',
    'Recurring successful invocations confirm the rope reading''s durability and low suppression; unanswered or punished invocations would indicate the accountability ran one way in practice, pushing effective extraction and suppression upward for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottom_up_enforcement_signal, empirical, 'Whether subordinate enforcement episodes evidence a working bargain or its collapse.').

omega_variable(
    kernel_codification_framing,
    'Is the Ma''at kernel correctly framed as distributed and under-specified (many texts and practices, no single adjudicator), or as a fixed text centered on the Spell 125 negative confession with credentialed interpreters, which would relocate authority in the priestly line?',
    'Survey the transmission history: whether the confession list, the instruction books, and courtroom oath practice behave as variants of one kernel or as a fixed canonical item with authorized expounders; examine variant sequences of the confession across coffins and papyri.',
    'Under the fixed-text framing, kernel_codification moves to fixed_text, authority_grounding moves toward lineage, the plurality claim that anchors this reading''s low extraction weakens, and the computed classification could shift toward a more extractive type; the current distributed framing is defensible but not forced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Alternative framings of the kernel''s codification produce different authority and extraction pictures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_distributed_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_distributed_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(maat_distributed_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(maat_distributed_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(maat_distributed_tr_t40, maat_order_principle__distributed_maintenance_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(maat_distributed_tr_t50, maat_order_principle__distributed_maintenance_reading, theater_ratio, 50, 0.17).
narrative_ontology:measurement(maat_distributed_tr_t60, maat_order_principle__distributed_maintenance_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(maat_distributed_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(maat_distributed_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(maat_distributed_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(maat_distributed_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(maat_distributed_be_t40, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(maat_distributed_be_t50, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(maat_distributed_be_t60, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 60, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(maat_distributed_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(maat_distributed_su_t10, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(maat_distributed_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(maat_distributed_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement(maat_distributed_su_t40, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement(maat_distributed_su_t50, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement(maat_distributed_su_t60, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 60, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, reciprocity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Ma'at' conflates at least three structurally distinct claims about who maintains cosmic order and who may interpret the standard; per the epsilon-invariance principle these are decomposed into separate constraint stories linked as a family. This file instantiates the distributed_maintenance_reading. The divine_mandate_reading is the upstream Old Kingdom claim from which the others historically descend; the distributed reading emerges downstream in the First Intermediate Period democratization and exerts structural pressure on both siblings, eroding the monopoly claim without resolving the dispute. Each member carries its own epsilon, beneficiaries, and classification; no member hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
