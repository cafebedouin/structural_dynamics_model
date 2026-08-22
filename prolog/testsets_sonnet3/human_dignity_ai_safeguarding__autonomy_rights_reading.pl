% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: human_dignity_ai_safeguarding__autonomy_rights_reading
 *   human_readable: Dignity-as-Autonomy Regulatory Reading (AI Governance)
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This story instantiates the autonomy-rights reading of the
 *   human_dignity_ai_safeguarding kernel: the claim that human dignity is
 *   grounded in autonomy, rationality, and rights rather than in divine image
 *   or in an open-ended capacity threshold. Under this reading, AI governance
 *   frameworks operationalize dignity through consent architecture,
 *   transparency mandates, and rights-based redress. The reading solves a
 *   genuine coordination problem for pluralistic societies (a shared,
 *   non-sectarian legal vocabulary) but the same autonomy-centered grounding
 *   structurally weakens protection for those who cannot perform legible
 *   rational consent — cognitively impaired persons, coerced gig workers, and
 *   communal societies whose personhood concepts are relational rather than
 *   individualist. This is authored as a single, ε-invariant claim about the
 *   standing arrangement as this reading's own advocates would describe it;
 *   the imago_dei_reading and posthumanist_reading are separate constraints
 *   with their own ε, authored in separate files.
 *
 * KEY AGENTS:
 *   - regulatory_agencies: institutional agenda-setter administering the autonomy-rights compliance regime
 *   - gig_economy_ai_workers: powerless payers whose formal consent substitutes for substantive protection
 *   - cognitively_impaired_persons: powerless payers whose dignity claim rests on capacity proxies rather than unconditional status
 *   - imago_dei_advocates and posthumanist_theorists: excluded voices contesting the anthropological ground from opposite directions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42).
domain_priors:suppression_score(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.38).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__autonomy_rights_reading, "Dignity-as-Autonomy Regulatory Reading (AI Governance)").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__autonomy_rights_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__autonomy_rights_reading, '98f73c91-5a1b-4518-916a-cd340de3b17b').
narrative_ontology:cs_kernel_codification('98f73c91-5a1b-4518-916a-cd340de3b17b', distributed).
narrative_ontology:cs_authority_grounding('98f73c91-5a1b-4518-916a-cd340de3b17b', distributed).
narrative_ontology:cs_reading_relation('98f73c91-5a1b-4518-916a-cd340de3b17b', human_dignity_ai_safeguarding__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('98f73c91-5a1b-4518-916a-cd340de3b17b', human_dignity_ai_safeguarding__posthumanist_reading, influences).
narrative_ontology:cs_axiom('98f73c91-5a1b-4518-916a-cd340de3b17b', foundational, dignity_constituted_by_rational_autonomy).
narrative_ontology:cs_axiom_status(dignity_constituted_by_rational_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('98f73c91-5a1b-4518-916a-cd340de3b17b', dignity_constituted_by_rational_autonomy, deontological).
narrative_ontology:cs_axiom('98f73c91-5a1b-4518-916a-cd340de3b17b', secondary, consent_as_sufficient_legitimation).
narrative_ontology:cs_axiom_status(consent_as_sufficient_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('98f73c91-5a1b-4518-916a-cd340de3b17b', consent_as_sufficient_legitimation, conventional).
narrative_ontology:cs_reference_frame('98f73c91-5a1b-4518-916a-cd340de3b17b', secular_liberal_rights_consensus).
narrative_ontology:cs_drift_state('98f73c91-5a1b-4518-916a-cd340de3b17b', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98f73c91-5a1b-4518-916a-cd340de3b17b', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, informed_data_subjects).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, ai_governance_consultancies).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, gig_economy_ai_workers).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, cognitively_impaired_persons).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, non_western_communal_societies).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, small_ai_developers).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, liberal_individualist_personhood).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, consent_based_legitimacy).
narrative_ontology:constraint_vindicates(human_dignity_ai_safeguarding__autonomy_rights_reading, rational_agency_as_moral_ground).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce AI governance frameworks (transparency mandates, consent requirements, data protection regimes) explicitly grounded in the claim that dignity consists in autonomous rational choice. They administer the compliance apparatus, certify conformity, and levy penalties for violation. Their institutional legitimacy and budget depend on the autonomy-rights framing remaining the operative anthropology in law.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, continental).

% Litigate, lobby, and publish reports using the autonomy-rights vocabulary of consent and self-determination to challenge AI deployments. Their funding, professional standing, and policy access are built on this anthropology being the reference frame regulators use; they gain standing and resources whenever it is codified into law.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_safeguarding__autonomy_rights_reading, rights_advocacy_organizations, agenda_setter).

% Literate, resourced individuals who can read consent forms, exercise opt-outs, and invoke rights language when an AI system harms them. The framework gives them real leverage: they can demand transparency, contest automated decisions, and withdraw data. Their capacity to exercise autonomy is what the whole architecture presumes.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, informed_data_subjects, beneficiary,
    moderate, biographical, mobile, national).

% Sell compliance auditing, consent-architecture design, and 'ethical AI' certification services built entirely around the autonomy-rights framework. They profit from the complexity of translating an abstract dignity claim into checklists and disclosure documents; a different anthropological grounding would require rebuilding their entire product line.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, ai_governance_consultancies, beneficiary,
    organized, biographical, arbitrage, global).

% Subject to algorithmic management and AI-mediated task allocation. They 'consent' to platform terms as a condition of earning income at all — the autonomy-rights framework treats their click-through agreement as a dignity-respecting exercise of rational choice, which forecloses labor-protection arguments that don't fit the consent template. Formal consent substitutes for substantive bargaining power they do not have.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, gig_economy_ai_workers, payer,
    powerless, immediate, trapped, global).

% Individuals whose capacity for the kind of explicit rational autonomy the framework presumes is diminished by disability, dementia, or developmental condition. Because the dignity claim is anchored to autonomous rational agency, their protections under this reading depend on proxies, guardianship constructs, and capacity assessments rather than on an unconditional status — a structurally weaker footing than a status grounded independent of capability.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, cognitively_impaired_persons, payer,
    powerless, civilizational, trapped, national).

% Societies whose ethical traditions ground personhood in relationship, kinship, and communal role rather than individual rational autonomy. International AI governance standards built on the autonomy-rights anthropology are exported through trade agreements and development conditionality, pressuring these societies to adopt individualist consent architectures that do not map onto their own dignity concepts, or be treated as non-compliant.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, non_western_communal_societies, payer,
    moderate, generational, constrained, regional).

% Smaller firms and open-source projects that must build consent-management infrastructure, transparency reporting, and rights-compliance documentation to operate legally. The compliance burden scales poorly with their size relative to well-resourced incumbents who can absorb it, so the framework's protective intent produces a market-structuring cost that falls disproportionately on them.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, small_ai_developers, payer,
    moderate, biographical, constrained, national).

% Religious and theological communities who hold that dignity is unconditional and prior to capability, grounded in being made in the divine image. They are largely absent from technical AI governance drafting rooms, which are staffed by secular rights lawyers and bioethicists; their objection — that grounding dignity in rational autonomy quietly excludes the profoundly disabled, the unborn, and the comatose from full protection — rarely reaches the regulatory text.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, imago_dei_advocates, excluded,
    organized, civilizational, constrained, global).

% Scholars and advocates who argue dignity should attach to any sufficiently autonomous or sentient entity regardless of biological humanity, including enhanced humans and synthetic minds. They are marginal to mainstream policy discourse, which treats the human/non-human boundary as settled by species membership rather than by the capacities the autonomy-rights reading itself claims to prize.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, posthumanist_theorists, excluded,
    moderate, civilizational, mobile, global).

% Analyze the coherence and consequences of grounding dignity in autonomy and rationality, tracing how the choice of anthropological foundation determines which beings get full protection and which get diminished or proxy protection under AI governance regimes.
narrative_ontology:constraint_stakeholder(human_dignity_ai_safeguarding__autonomy_rights_reading, philosophical_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_dignity_ai_safeguarding__autonomy_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(human_dignity_ai_safeguarding__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, religiously-neutral, litigable common ground for regulating AI systems across pluralistic societies: consent, transparency, and rights language can be operationalized into statutes and audits without requiring agreement on theology or metaphysics.
% TRANSFER_FUNCTION: Moves regulatory legitimacy, compliance revenue, and protective leverage toward those who can perform rational autonomous consent (informed subjects, advocacy groups, consultancies) and away from those whose dignity claim does not fit the autonomy template (impaired persons, coerced workers, communal societies) — their protection becomes conditional on proxies or capacity fictions rather than unconditional.
% ABSENT_VOICES: Imago dei theological traditions and posthumanist theorists are structurally outside the drafting process; the former would object that autonomy-grounding quietly ranks human worth by capability, the latter that it arbitrarily fixes the boundary of moral status at species membership rather than capacity.
% DISAPPEARANCE_RATIONALE: Regulators and rights organizations would say the entire consent/transparency apparatus collapses without the autonomy-rights anthropology underneath it — the world of AI law reorganizes. Theological critics would say the underlying moral reality of human worth is untouched; only a particular (and in their view impoverished) legal vocabulary for describing it disappears, and a truer grounding could replace it without loss.
% FOUNDING_PROBLEM: Pluralistic, secular societies needed a shared, non-sectarian basis for AI governance that did not require consensus on contested metaphysical or religious claims about human nature, while still constraining increasingly powerful automated decision systems.
% FOUNDING_PROBLEM_CORROBORATION: Human rights lawyers and secular bioethicists (inside the framework) attest the problem is live and well-served. Disability rights scholars and theological ethicists (partially outside the benefiting coalition) attest the problem has been solved unevenly — the framework serves the capable well and leaves those who fail the autonomy test with weaker, proxy-mediated protection, which is a different and arguably worse problem than the one it set out to solve.
narrative_ontology:disappearance_verdict(human_dignity_ai_safeguarding__autonomy_rights_reading, contested).
narrative_ontology:founding_problem_status(human_dignity_ai_safeguarding__autonomy_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).
:- end_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and rising: the framework genuinely coordinates AI oversight across secular pluralistic jurisdictions, but the autonomy template increasingly transfers protective weight toward the capable at the expense of those who fail the consent test. Suppression is moderate (0.38) — enforcement exists (compliance audits, penalties) but is not severe; the deeper exclusion operates through conceptual foreclosure (proxy protections, capacity fictions) rather than raw coercion. Theater ratio (0.3) reflects a real but partially performative compliance industry (consultancies certifying 'ethical AI' via checklist consent flows that do not track substantive protection).
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/advocacy seat, the arrangement looks like rope: a hard-won, workable, secular consensus that protects people through enforceable rights. From the trapped-payer seat (gig workers, impaired persons), the same structure looks like a filter that determines who counts as fully protected — the engine should show this divergence as a computed consequence of the differing power/exit data, not as an authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies, advocacy organizations, and consultancies sit near the beneficiary end: they set, profit from, or gain standing through the autonomy-rights vocabulary being the operative legal anthropology. Informed data subjects benefit close to symmetrically — genuine leverage, real costs of vigilance. Gig workers and cognitively impaired persons sit near the full-target end: trapped exit, and the framework's own logic (consent-as-dignity) is what forecloses stronger protection for them. Non-western communal societies and small developers are targets of a different kind — externally imposed compliance costs rather than direct extraction, hence moderate rather than powerless positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a shared secular basis for AI law) remains partly live — pluralistic societies still need non-sectarian coordination — but the framework has also accumulated a second, unintended function: quietly ranking moral status by rational capacity. Classifying this as tangled_rope rather than snare or rope preserves both truths: real coordination value for the coordinating class, real and structurally embedded cost for those who cannot perform autonomous consent. A pure rope or pure snare label would erase one half of this structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_arbitrariness,
    'Is grounding dignity in rational autonomy a principled anthropological claim, or a legally convenient proxy that happens to track the interests of those already positioned to exercise formal consent?',
    'Compare protective outcomes for populations at the margins of the autonomy criterion (severe cognitive disability, infancy, coma) under this reading versus jurisdictions operating under unconditional-status frameworks; a persistent protection gap correlated with capacity would support the proxy-convenience reading.',
    'If the autonomy grounding is shown to systematically under-protect low-capacity persons relative to unconditional-status alternatives, this reading''s coordination claim is undercut and its extractive character (asymmetric protection by capability) becomes the dominant structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_arbitrariness, conceptual, 'Whether autonomy-as-dignity-ground is principled or a capability-tracking proxy.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does codifying the autonomy-rights anthropology into binding AI law structurally foreclose the imago_dei_reading''s unconditional-status protections for the same populations, or can the two coexist in parallel legal instruments (e.g., disability rights statutes layered on top)?',
    'Trace whether jurisdictions that adopt autonomy-rights AI statutes retain or erode independent unconditional-dignity protections (constitutional human dignity clauses, disability rights law) over the following decade.',
    'If unconditional protections erode as the autonomy-rights framework becomes dominant, the relation to imago_dei_reading is closer to foreclosing in practice even though the two are not logically contradictory in principle; if they persist independently, the relation is genuinely just influences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, empirical, 'Whether autonomy-rights codification erodes unconditional-status protections in practice.').

omega_variable(
    posthumanist_boundary_pressure,
    'If dignity is grounded in rational autonomy and rights rather than species membership, does logical consistency eventually require extending protection to sufficiently autonomous non-human or synthetic agents — pulling this reading toward the posthumanist_reading over time?',
    'Track whether advanced AI systems demonstrating autonomy-like behavior begin to receive rights-adjacent protections under autonomy-rights legal frameworks, and whether drafters explicitly resist or embrace this extension.',
    'If the autonomy criterion is applied consistently, this reading has latent structural pressure toward the posthumanist_reading; if drafters carve out a species-based exception, the reading is revealed to rest on an unstated additional axiom (human species membership) not disclosed in its stated grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(posthumanist_boundary_pressure, conceptual, 'Latent logical pressure from autonomy-grounding toward posthumanist dignity extension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__autonomy_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(huma_tr_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(huma_tr_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(huma_tr_t18, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(huma_tr_t24, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(huma_be_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(huma_be_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(huma_be_t18, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement(huma_be_t24, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(huma_su_t6, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 6, 0.26).
narrative_ontology:measurement(huma_su_t12, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(huma_su_t18, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 18, 0.33).
narrative_ontology:measurement(huma_su_t24, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__imago_dei_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__posthumanist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language 'human dignity and AI safeguarding' concept under the human_dignity_ai_safeguarding kernel. The autonomy_rights_reading (this file) authors moderate, rising extraction concentrated on those who cannot perform legible rational consent. The imago_dei_reading authors dignity as unconditional and prior to capability, with a structurally different (lower, differently-distributed) extraction profile. The posthumanist_reading authors dignity as capacity-general across substrates, with its own distinct beneficiary/victim structure (enhanced/synthetic persons as beneficiaries, unenhanced humans and legacy institutions as relative payers). Each story carries its own ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
