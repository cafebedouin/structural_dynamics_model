% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity: Pharaonic Obligation and Cosmic Balance
 *   domain: political_philosophy/religious_studies/ancient_history
 *
 * SUMMARY:
 *   The Ma'at reciprocity reading frames the Pharaoh's rule as grounded in
 *   mutual obligation: the Pharaoh provides justice, stable resource
 *   distribution, and cosmic order maintenance; in return, the population
 *   grants legitimacy, taxes, and labor. The priesthood interprets and
 *   enforces compliance with this reciprocity norm. This reading claims the
 *   constraint as tangled_rope: genuine coordination function (centralized
 *   irrigation and dispute resolution) combined with asymmetric extraction
 *   (the Pharaoh bears resource costs; the priesthood gains interpretive
 *   authority; the common population gets goods but is trapped in
 *   dependency). The reading is distinct from the divine_mandate reading
 *   (Pharaoh embodies Ma'at and cannot violate it) and the
 *   distributed_maintenance reading (all actors maintain Ma'at through proper
 *   conduct). This reciprocity reading emphasizes accountability: if the
 *   Pharaoh fails to distribute grain or maintain justice, the priesthood and
 *   elites can legitimately declare him in violation and withdraw support.
 *   Measurement data show extractiveness rising slightly over time
 *   (0.38→0.45) as later periods saw more ceremonial theater and less actual
 *   resource distribution relative to political claims, but the constraint
 *   remained below the snare threshold because the reciprocity obligation was
 *   still invoked as an enforceable standard.
 *
 * KEY AGENTS:
 *   - Pharaoh (office): administrator and enforcer of Ma'at reciprocity; bears resource distribution costs; subject to priesthood interpretation of compliance
 *   - Priesthood (Ma'at interpreters): organized authority interpreting cosmic balance requirements; can declare Pharaoh in violation; benefits from interpretive monopoly
 *   - Common population: receives grain distribution and dispute resolution; trapped geographically and by dependency; collective beneficiary but powerless as individuals
 *   - Administrative elites: implement Pharaoh's obligations; can withdraw support if obligations neglected
 *   - Rival factions: excluded from formal reciprocity frame but use legitimacy breaches as pretext for succession challenges
 *   - Cosmological order (Ma'at concept): abstract referent grounding the obligation system's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.38).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity: Pharaonic Obligation and Cosmic Balance").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "political_philosophy/religious_studies/ancient_history").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'd1aa9d28-d42a-4acf-8245-43f5e8d87df3').
narrative_ontology:cs_kernel_codification('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', fixed_text).
narrative_ontology:cs_authority_grounding('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', lineage).
narrative_ontology:cs_interpretation_layer_present('d1aa9d28-d42a-4acf-8245-43f5e8d87df3').
narrative_ontology:cs_reading_relation('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', foundational, pharaoh_subject_to_ma_at_mutual_obligation).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_ma_at_mutual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', pharaoh_subject_to_ma_at_mutual_obligation, deontological).
narrative_ontology:cs_axiom('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', foundational, priesthood_authorized_compliance_arbiter).
narrative_ontology:cs_axiom_status(priesthood_authorized_compliance_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', priesthood_authorized_compliance_arbiter, conventional).
narrative_ontology:cs_reference_frame('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', reciprocal_pharaonic_obligation_frame).
narrative_ontology:cs_drift_state('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', late_dynastic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1aa9d28-d42a-4acf-8245-43f5e8d87df3', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, priesthood_ma_at_interpreters).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, common_population).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, pharaoh_office).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, administrative_elites).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, administrative_elites).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, cosmic_order_requires_mutual_obligation).
narrative_ontology:constraint_vindicates(maat_order_principle__reciprocity_reading, ruler_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Pharaoh administers and enforces the Ma'at system: distributes grain from royal stores, adjudicates disputes, maintains irrigation and temple infrastructure, and legitimates rule through justice provision. Bound by the reciprocity norm to provide these goods continuously or face loss of divine sanction and elite support withdrawal. Bears the costs of resource distribution and active governance directly from state resources.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh_office, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, pharaoh_office, payer).

% Interprets and adjudicates Ma'at compliance: determines whether the Pharaoh has met obligations, articulates what cosmic balance requires at any moment, and can declare the Pharaoh in violation. Benefits from the constraint's existence because it grants them authority to hold the Pharaoh accountable and advise elites on legitimacy questions. Can withdraw interpretive support or declare a new Pharaoh if obligations are neglected, or shift interpretation to suit political conditions.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priesthood_ma_at_interpreters, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, priesthood_ma_at_interpreters, agenda_setter).

% Receives grain distributions, dispute resolution, and irrigation maintenance from the Pharaoh in exchange for taxes, labor service, and loyalty. Trapped geographically and by kinship to the Nile valley and royal authority. Collects the concrete goods the reciprocity norm requires. Cannot exit or enforce the obligation themselves; dependence flows through priesthood and local elites.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, common_population, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from the orderly resource distribution system and intact institutional hierarchy that reciprocal obligation maintains. Pay by implementing the Pharaoh's obligations (distributing grain, managing irrigation, enforcing law). Can withdraw support from a Pharaoh who neglects obligations, or pressure the priesthood to declare non-compliance.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, administrative_elites, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, administrative_elites, payer).

% Would benefit from a framework where Pharaonic obligation is not enforced or is enforceable by competing claimants. Excluded from the formal reciprocity structure but use legitimacy breaches (claimed violation of Ma'at) as a pretext to challenge the ruling Pharaoh's authority and mobilize priesthood and elite support.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, rival_or_usurper_faction, excluded,
    powerful, biographical, trapped, national).

% The abstract principle of Ma'at itself: the cosmos requires balance and reciprocal obligation to prevent chaos. This is not an agent in the social sense but the normative referent the entire constraint system invokes. It anchors the legitimacy claim that obligation is not arbitrary but cosmic requirement.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, cosmological_order_concept, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__reciprocity_reading, cosmological_order_concept).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, priesthood_ma_at_interpreters).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system where the Pharaoh's rule is legitimated by active provision of public goods (grain, justice, irrigation infrastructure) rather than pure coercion. Society coordinates around the expectation that legitimate rule requires reciprocal resource distribution and dispute resolution. The Pharaoh coordinates elite and popular compliance through this exchange; without it, the state fragments into warlordism and famine.
% TRANSFER_FUNCTION: Moves grain, irrigation maintenance, and dispute-resolution authority FROM the Pharaoh's administration TO the general population and local authorities. In return, the population provides taxes, labor service, and legitimacy-conferring loyalty. The priesthood receives interpretive authority over Ma'at compliance and gains influence over succession questions when obligations are breached.
% ABSENT_VOICES: Enslaved populations (used in construction and some agricultural work) are excluded from the reciprocity frame—they receive no share of distributed goods proportional to their labor contribution and cannot voice claims on the Pharaoh through legitimate channels. Competing religious systems or foreign alternatives are excluded by the cosmological monopoly claim that Ma'at is the only legitimate ordering principle.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation disappeared, the Pharaoh would face no institutionalized constraint to redistribute grain; famine and hoarding would follow. The priesthood would lose leverage to hold the Pharaoh accountable. Elites would have no shared framework for evaluating legitimacy or coordinating against an abusive ruler. The centralized irrigation system would degrade without maintenance obligations. Society would reorganize around either warlordism or a different legitimacy system (e.g., pure divine mandate or distributed maintenance).
% FOUNDING_PROBLEM: Early Nile-based settlement required centralized management of the flood: irrigation canals, grain storage, dispute resolution over water and land rights. No single actor could sustain this without ongoing cooperation. The reciprocity frame solved the problem by making cooperation a cosmic obligation the Pharaoh must uphold and the priesthood must certify—breaking the cycle of warlordism that had preceded unification.
% FOUNDING_PROBLEM_CORROBORATION: Egyptologists studying irrigation inscriptions and Nile flood variability confirm that centralized grain management was structurally necessary to prevent famine during low-flood years (a recurrent crisis); the founding problem did not disappear in later periods when the constraint persisted. Administrative documents show that when Pharaohs failed to distribute grain (as during the First Intermediate Period), the state fragmented and regional warlords emerged—confirming the disappearance verdict.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the constraint genuinely coordinates public goods provision and dispute resolution—the reciprocity norm prevents pure predation. However, extraction rises because the Pharaoh does bear resource costs while the priesthood captures interpretive authority; this asymmetry marks it as tangled_rope rather than rope. Suppression is moderate-low (0.38) because the reciprocity frame legitimates the arrangement in the eyes of the population and elites—coercion is needed primarily to prevent succession challenges when obligations are neglected, not to enforce compliance with a widely resented system. Theater_ratio starts low (0.15) and rises slightly (to 0.22) as later dynasties invested more in ceremonial affirmation of Ma'at while actual resource distribution stagnated—a classic drift toward theatrical maintenance of a norm while its functional content atrophied. Accessibility_collapse is high (0.72) because once the Ma'at framework is understood, alternative legitimacy systems are cosmologically foreclosed (the priesthood enforces the monopoly on cosmic interpretation); this high collapse reflects the power of cosmological framing to make alternatives unthinkable, not purely coercive barriers. Resistance is moderate (0.58) because the reciprocity reading itself acknowledges the Pharaoh's obligation: during periods when distribution failed, resistance was substantial (documented elite factionalism, succession instability); the constraint permits legitimate resistance if obligations are breached, which distinguishes it from a pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's seat: the constraint is a binding obligation anchored in cosmic necessity; failing to meet it (famine, injustice) produces legitimacy collapse and succession crisis. From the priesthood's seat: the constraint is an interpretive opportunity; they hold the power to certify or deny the Pharaoh's compliance, which makes them indispensable advisors and succession arbiters. From the common population's seat: the constraint is a protection (they have a claim on the Pharaoh's grain stores) but also a trap (they cannot exit the dependency). From the administrative elites' seat: the constraint is both burden (they must implement distribution) and tool (they can pressure the Pharaoh by invoking reciprocity norms). The engine computes these seat-level type divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh sits at high d (near 1.0 / full target): administrative responsibility for resource distribution, constrained exit (cannot simply abandon the office without state collapse), institutional power limited by priesthood's interpretive authority. The priesthood sits at moderate-low d (near 0.4): they benefit interpretively and politically but are not extracted from; they have mobile exit options (shift allegiance to a different Pharaoh). The common population sits at moderate d (near 0.5): genuine benefit from distributed goods balanced against trapped exit and dependency on Pharaoh's goodwill; they are neither pure beneficiaries nor targets. Administrative elites sit at moderate d (near 0.45): they pay through implementation labor but benefit from system stability; constrained exit (tied to state hierarchy). The reciprocity reading produces this structure because it makes the Pharaoh's obligation to distribute concrete and verifiable—hence the Pharaoh is the primary target. If the divine_mandate reading were adopted, d would invert (Pharaoh is beneficiary, population is subordinate); if distributed_maintenance were adopted, d would flatten (every actor responsible in their station).
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading avoids the mandatrophy trap because the founding problem (centralized irrigation and famine prevention) remained live throughout Egyptian history. Even when extractiveness rose and theater increased, the underlying problem—that dispersed settlement cannot survive Nile flood variance without centralized coordination—never disappeared. The constraint is therefore genuinely functional coordination, not a zombie arrangement persisting through theatrical performance alone. However, late-period data show rising theater_ratio (increasing ceremonial reaffirmation, decreasing actual distribution per capita), which signals incipient drift toward pitonification. If late dynasties had maintained the theater while completely abandoning distribution, the constraint would cross into mandatrophy (live founding problem but dead functional response). The reciprocity reading's strength is that it anchors the mandate to reciprocal obligation, not to Pharaonic divinity; this makes non-compliance visible and triggers resistance, which has historically renewed the functional response rather than allowing pure theatrical drift. The distributed_maintenance reading, by contrast, would carry higher mandatrophy risk because it distributes responsibility so broadly that no single actor is accountable if coordination fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_divine_mandate_framing,
    'Is the Pharaoh subject to Ma''at constraints as a mutual obligation he must fulfill to maintain legitimacy, or does he EMBODY Ma''at itself such that his actions are by definition Ma''at-compliant?',
    'Historical record: when Pharaohs neglected grain distribution or failed in justice provision, did the priesthood declare them in violation of Ma''at (reciprocity reading) or redefine Ma''at to accommodate their conduct (divine mandate reading)? Succession struggles reveal which reading was operational—if priesthood used non-compliance to legitimate a new Pharaoh, reciprocity reading was active; if they affirmed continuity regardless of performance, divine mandate reading was active.',
    'If reciprocity reading is correct, the constraint permits legitimate resistance and allows for Pharaonic accountability; the engine would compute higher d for the common population (they have a legitimate complaint mechanism) and lower d for the Pharaoh (subject to external judgment). If divine mandate reading is correct, the Pharaoh''s actions are beyond critique by definition; d would invert and the constraint would reclassify as closer to snare or mountain (depending on whether divine mandate is perceived as natural or constructed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_mandate_framing, conceptual, 'Whether Ma''at obligation is reciprocal (mutual, enforceable) or absolute (emanating from the Pharaoh''s cosmic status).').

omega_variable(
    priesthood_independence_extraction,
    'Do the priests genuinely interpret and enforce the reciprocity norm independently, or do they function as agents of the Pharaoh''s will disguised in cosmological language?',
    'Documented cases where priesthood refused to legitimate a Pharaoh''s conduct or backed a succession challenge against the sitting Pharaoh. If such cases exist and are numerous, priesthood has independent authority; if rare or absent, priesthood interpretation is a rubber-stamp and the extraction is unidirectional (Pharaoh extracts through priestly cover).',
    'If priests are independent, the priesthood''s role as beneficiary and agenda_setter is real, and the constraint is genuinely tangled_rope (coordination with priesthood capturing interpretive rents). If priests are purely Pharaonic agents, the constraint simplifies toward snare (extraction masked by cosmological theater); the priesthood would be payer, not beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_independence_extraction, empirical, 'Whether priesthood interpretation of Ma''at reciprocity is autonomous or subordinate to Pharaonic control.').

omega_variable(
    distributed_maintenance_interpretive_shift,
    'Could the distributed_maintenance_reading be the same constraint viewed from a different social level (elite vs. commoner perspective), or is it a genuinely alternative framing of the kernel?',
    'Textual evidence: do sources that advocate distributed maintenance (e.g., wisdom literature, non-elite temple inscriptions) describe the same obligation structure as reciprocity texts, or do they describe different structural arrangements? Do they coexist as simultaneous doctrines or compete historically?',
    'If coexisting doctrines describing the same structure: readings are perspectival variants (same constraint viewed from different seats) and should be authored as one story with multiple measured perspectives—reclassify as a methodological choice. If genuinely alternative structures: this confirms they are separate constraints as authored; each has its own ε, and the network link is causally interpretive (distributed_maintenance emerges when reciprocity breaks down).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_maintenance_interpretive_shift, conceptual, 'Whether distributed_maintenance is an alternative framing of Ma''at obligation or a different constraint entirely.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.38) structural (enforced by priesthood and elite apparatus) or internalized (the population believes Ma''at reciprocity is cosmically inevitable and unquestionably legitimate)?',
    'Post-order collapse behavior: when the Pharaoh collapsed and distributed maintenance failed (First Intermediate Period, Second Intermediate Period), did the population immediately reorganize around a new reciprocity system, or did they attempt distributed or leaderless alternatives? Rapid re-adoption of reciprocity structure suggests internalization; experimentation with alternatives suggests structural suppression.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the scalar measure suggests—the population carries the obligation frame with them even without institutional enforcement. If structural, suppression is accurate at 0.38 and would drop if coercive apparatus failed. This affects how durable the constraint is under stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural (enforced by authorities) or internalized (believed as cosmically necessary).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__reciprocity_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(maat_tr_t5, observed).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__reciprocity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(maat_tr_t10, observed).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__reciprocity_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(maat_tr_t15, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(maat_tr_t20, observed).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__reciprocity_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(maat_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__reciprocity_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(maat_be_t5, observed).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__reciprocity_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(maat_be_t10, observed).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__reciprocity_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement_basis(maat_be_t15, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(maat_be_t20, observed).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__reciprocity_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement_basis(maat_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__reciprocity_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(maat_su_t5, observed).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__reciprocity_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(maat_su_t10, observed).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__reciprocity_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(maat_su_t15, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(maat_su_t20, observed).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__reciprocity_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(maat_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.18).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three structurally distinct constraints: (1) reciprocity_reading (this story) frames Ma'at as mutual obligation with priesthood as arbiter—moderate extraction ceiling. (2) divine_mandate_reading frames Ma'at as cosmic status flowing through the Pharaoh—higher extraction ceiling, no external accountability. (3) distributed_maintenance_reading distributes responsibility to all actors—lower extraction ceiling per actor, diffuse accountability. Each reading has different ε (extraction varies with whether Pharaoh is accountable), different beneficiary/victim structure (priesthood role changes), and different type classification. ε-invariance principle: the readings instantiate different constraints because changing the reading changes which obligations are enforced and who can enforce them—not merely a measurement choice but a different standing arrangement. All three are part of the maat_order_principle family and linked via network.affects_constraints; this reading influences the others because reciprocity was the dominant operational frame in the sources and established the baseline against which alternative framings are interpreted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
