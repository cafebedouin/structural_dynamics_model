% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text Read as Structurally Enabling Uncompensated Extraction (Copyleft Counterfactual)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the copyleft-counterfactual reading of the
 *   permissive-license-text kernel: the same license clause (no reciprocity
 *   requirement on derivative works) that the commons-coordination reading
 *   treats as friction-minimization and the corporate-moat reading treats as
 *   extraction-enablement is here read as a structural gap whose remedy is
 *   known and available — viral reciprocity as embodied in the GPL. The
 *   distinguishing move of this reading is not merely diagnosing extraction
 *   (the corporate-moat reading also does that) but asserting that the
 *   extraction is a CHOICE built into the license text, evidenced by the
 *   counterfactual: a reciprocity clause would foreclose exactly the
 *   extraction path this story documents. Original maintainers and unpaid
 *   contributors supply labor into a commons that permissive terms leave open
 *   to uncompensated commercial capture, and the story treats copyleft
 *   licensing as the coordination mechanism the commons should have adopted
 *   rather than a purely ideological alternative.
 *
 * KEY AGENTS:
 *   - proprietary_derivative_vendors: institutional beneficiary capturing commercial value from unreciprocated derivative use
 *   - cloud_service_resellers: institutional beneficiary monetizing hosted permissive-licensed infrastructure
 *   - original_permissive_maintainers: trapped payer bearing unfunded maintenance burden and irreversible licensing choice
 *   - unpaid_contributor_pool: powerless payer whose labor becomes uncompensated commercial input
 *   - gpl_copyleft_advocates: excluded voice naming the reciprocity gap as the specific structural mechanism
 *   - end_users_of_derivative_products: moderate-power beneficiary of the resulting commercial products, largely unaware of upstream strain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.71).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.42).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text Read as Structurally Enabling Uncompensated Extraction (Copyleft Counterfactual)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '1510df4c-f404-486a-a42a-7a2071bf3c47').
narrative_ontology:cs_kernel_codification('1510df4c-f404-486a-a42a-7a2071bf3c47', fixed_text).
narrative_ontology:cs_authority_grounding('1510df4c-f404-486a-a42a-7a2071bf3c47', practice).
narrative_ontology:cs_interpretation_layer_present('1510df4c-f404-486a-a42a-7a2071bf3c47').
narrative_ontology:cs_reading_relation('1510df4c-f404-486a-a42a-7a2071bf3c47', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1510df4c-f404-486a-a42a-7a2071bf3c47', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('1510df4c-f404-486a-a42a-7a2071bf3c47', foundational, reciprocity_necessity_thesis).
narrative_ontology:cs_axiom_status(reciprocity_necessity_thesis, holdable).
narrative_ontology:cs_axiom_grounding('1510df4c-f404-486a-a42a-7a2071bf3c47', reciprocity_necessity_thesis, instrumental).
narrative_ontology:cs_axiom('1510df4c-f404-486a-a42a-7a2071bf3c47', foundational, uncompensated_labor_capture_is_objectionable).
narrative_ontology:cs_axiom_status(uncompensated_labor_capture_is_objectionable, holdable).
narrative_ontology:cs_axiom_grounding('1510df4c-f404-486a-a42a-7a2071bf3c47', uncompensated_labor_capture_is_objectionable, deontological).
narrative_ontology:cs_reference_frame('1510df4c-f404-486a-a42a-7a2071bf3c47', adoption_maximization_founding_rationale).
narrative_ontology:cs_drift_state('1510df4c-f404-486a-a42a-7a2071bf3c47', mature_cloud_saas_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1510df4c-f404-486a-a42a-7a2071bf3c47', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, cloud_service_resellers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, original_permissive_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, unpaid_contributor_pool).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, end_users_of_derivative_products).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Take permissively licensed code, incorporate it into closed proprietary products or hosted services, and sell the result without contributing modifications back or paying the original authors. The license text imposes no reciprocity obligation, so this path is fully lawful and requires no negotiation with upstream maintainers.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Repackage permissively licensed server software as a managed hosted offering, capturing the commercial value of operating it at scale while the original project receives no share of the resulting revenue and no obligation to fund its maintenance.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, cloud_service_resellers, beneficiary,
    institutional, generational, arbitrage, global).

% Wrote and continue to maintain the software under a license they often chose specifically to maximize adoption, but now bear the ongoing burnout, unfunded support burden, and competitive disadvantage of watching well-capitalized firms monetize their labor. Relicensing retroactively is legally and practically foreclosed once code has propagated; the choice was effectively made once, irreversibly.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, original_permissive_maintainers, payer,
    moderate, biographical, trapped, global).

% Volunteer developers who submit patches and features under the expectation of open collaborative benefit, not realizing (or unable to prevent) that their unpaid contributions become the raw input for downstream proprietary products they will never see revenue from or governance rights over.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, unpaid_contributor_pool, payer,
    powerless, biographical, constrained, global).

% Argue from outside the permissive-license governance conversation that viral reciprocity clauses (as in the GPL) are the structurally necessary corrective — any derivative that incorporates the code must also be released under equivalent terms, closing the extraction path. They are not consulted when projects choose permissive terms and are frequently dismissed as ideologically rigid rather than treated as naming a real structural vulnerability.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, gpl_copyleft_advocates, excluded,
    organized, generational, mobile, global).

% Receive functional, often cheaper or free-tier products built on the permissively licensed commons, without visibility into whether the underlying project is sustainably funded or slowly collapsing under maintenance load.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, end_users_of_derivative_products, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, end_users_of_derivative_products, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive terms genuinely lower the transaction cost of adoption — any party can incorporate the code into any kind of product, commercial or otherwise, without legal negotiation, enabling broad and rapid uptake of shared infrastructure.
% TRANSFER_FUNCTION: Moves engineering labor and maintenance investment from original authors and volunteer contributors to downstream commercial integrators, who capture the resulting commercial value without a reciprocal obligation to share improvements, funding, or governance back to the source.
% ABSENT_VOICES: GPL/copyleft advocates who would argue that the absence of a reciprocity clause is not a neutral design choice but a structural invitation to extraction; they are treated as an ideological faction rather than as identifying the specific mechanism (no viral obligation) that permits the transfer.
% DISAPPEARANCE_RATIONALE: If permissive terms were retroactively replaced with reciprocity requirements, proprietary derivative vendors and cloud resellers would face a binary choice between releasing their modifications/products under equivalent terms or ceasing to use the code — commercial software supply chains built on today's permissive commons would need to renegotiate licensing, fork away from the ecosystem, or begin compensating maintainers directly.
% FOUNDING_PROBLEM: Early open-source permissive licensing was built to solve a corporate-adoption problem: firms would not touch code encumbered by reciprocity obligations they feared would 'infect' their proprietary IP, so permissive terms were designed to maximize adoption by removing that friction entirely.
% FOUNDING_PROBLEM_CORROBORATION: Foundation-affiliated licensing historians and corporate legal counsel attest the adoption-maximization problem is still live and permissive terms remain the correct solution. Independent maintainer-sustainability researchers and unaffiliated economic studies of open-source funding gaps (e.g. surveys of unpaid maintainer burnout) attest, from outside both the corporate-beneficiary and maintainer-victim sets, that the original friction-reduction problem has been solved so thoroughly that a second, unaddressed problem — uncompensated extraction — has emerged in its place.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 by interval end) because the transfer from unpaid labor to commercial capture is direct and growing as the ecosystem of derivative commercial products matures. Suppression is moderate (0.42) — no one is coerced into contributing, but the norms of open-source participation, reputational lock-in, and sunk community investment make walking away from an established permissive project costly for maintainers. Theater ratio rises over the interval (0.15 to 0.38) as 'open source sustainability' initiatives (badges, foundation sponsorship programs, corporate 'giving back' PR) increasingly substitute symbolic acknowledgment for the structural fix (reciprocity obligations) that would actually close the extraction path.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/reseller seat, the license text is a clean legal instrument enabling legitimate commercial use of freely offered code — no wrongdoing, no coercion, full compliance with stated terms. From the maintainer/contributor seat, the same text is the mechanism by which their labor is legally but non-reciprocally converted into someone else's revenue. The engine computes these as structurally different experiences of the identical clause; this story does not adjudicate between them but authors the structural data (beneficiary/victim declarations, exit options) that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (proprietary derivative vendors, cloud resellers) sit near the full-beneficiary end: they have arbitrage-grade exit (can walk to another permissively licensed project or fork if terms ever tighten) and capture commercial value without corresponding obligation. Original maintainers and the unpaid contributor pool sit near the full-target end: their labor is the extracted resource, their exit is trapped or constrained (a maintainer cannot retroactively relicense already-forked code; a contributor's already-merged patches cannot be recalled), and they bear the ongoing cost of an arrangement they may have entered before its extraction dynamic was visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — corporate reluctance to adopt reciprocity-encumbered code — was real and has been solved: permissive licensing achieved massive adoption. But this reading holds that the arrangement's continuation past that founding problem, without ever introducing a reciprocity mechanism as the ecosystem matured, converted a coordination solution into a standing extraction channel. This is precisely the tangled_rope signature: coordination function (adoption-maximization) is genuine and still operating, but it now runs alongside asymmetric extraction requiring active enforcement (permissive license terms are legally enforced against attempts to impose retroactive reciprocity) to hold in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_necessity_vs_adoption_tradeoff,
    'Would imposing a viral reciprocity requirement on the projects now experiencing uncompensated extraction have actually achieved comparable adoption, or would it have suppressed the ecosystem growth that makes the code valuable enough to extract from in the first place?',
    'Comparative adoption-curve analysis between structurally similar permissively-licensed and copyleft-licensed projects (e.g. BSD-licensed vs. GPL-licensed infrastructure software in comparable niches) controlling for project age and initial community size.',
    'If copyleft projects achieve comparable adoption without the extraction dynamic, this reading''s core claim (reciprocity is a costless available fix) is strongly supported. If copyleft projects show significantly suppressed adoption, the tradeoff is real and this reading understates the coordination cost copyleft would impose, weakening the tangled_rope classification toward a more sympathetic rope reading of the status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_vs_adoption_tradeoff, empirical, 'Whether copyleft reciprocity is a free structural fix or trades off against the adoption goal the original permissive choice was solving for.').

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement among the three sibling readings of this kernel located in differing empirical predictions about adoption/extraction tradeoffs, or in differing normative commitments about whether uncompensated commercial use of freely offered labor is objectionable at all?',
    'Structured elicitation of commons_coordination_reading and corporate_moat_reading proponents asking them to state, in advance, what empirical evidence (if any) would change their normative conclusion about the license clause.',
    'If the disagreement is purely normative (whether extraction from volunteers is wrong), no empirical study resolves it and the three readings will remain permanently coexisting rather than convergent. If it is substantially empirical (disputed facts about adoption elasticity or actual revenue capture), targeted data collection could shift practitioner consensus toward one reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the three-reading kernel contest is an empirical dispute or a values dispute wearing empirical clothing.').

omega_variable(
    maintainer_consent_at_licensing_time,
    'Did original maintainers who chose permissive licensing genuinely anticipate and accept the extraction dynamic as a cost of maximizing adoption, or did they lack visibility into how commercial cloud/SaaS business models would later monetize their code at scale?',
    'Historical review of stated licensing rationale in project founding documentation, mailing lists, and contemporaneous interviews from the period when permissive terms were chosen, compared against the commercial landscape at that time versus today.',
    'If maintainers made an informed bet that adoption benefits would outweigh extraction costs, the victim framing in this story is weaker — they got what they knowingly signed up for. If the commercial extraction mechanisms (cloud resale in particular) were not foreseeable at licensing time, the victim framing and the extraction classification are substantially strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_consent_at_licensing_time, empirical, 'Whether the extraction was a knowingly accepted tradeoff or an unforeseen consequence of licensing choices made under different market conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perm_tr_t4, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perm_be_t4, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perm_su_t4, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(perm_su_t8, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(perm_su_t16, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(perm_su_t24, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the permissive_license_text kernel. commons_coordination_reading reads the same clause as low-epsilon coordination (friction minimization for universal implementation freedom); corporate_moat_reading reads it as high-epsilon extraction centered on vendor capture without emphasizing an available remedy; this reading (copyleft_counterfactual_reading) reads it as high-epsilon tangled_rope specifically because a structural alternative (GPL-style viral reciprocity) exists and was not adopted. All three share the same underlying license-clause kernel but instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim sets, and different classifications — per the epsilon-invariance principle they are authored as separate stories linked here rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
