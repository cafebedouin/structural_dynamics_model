% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Commons Institution Reading)
 *   domain: intellectual_property/software_governance/open_source
 *
 * SUMMARY:
 *   The GPL's reciprocity obligation (copyleft) is read here as institutional
 *   technology sustaining a commons against enclosure. Under this reading,
 *   the constraint's primary function is preventing proprietary firms and
 *   individual developers from capturing GPL-licensed code improvements for
 *   private use. The reciprocity obligation is not primarily about user
 *   freedom (that is the sibling freedom_reading) or about restricting
 *   business models (that is the restriction_reading); it is about
 *   maintaining a collectively-governed shared resource. Beneficiary: the
 *   commons-as-institution (which is not an actor but a structural
 *   arrangement). Victims: proprietary exit-maximizers (firms that want to
 *   privatize GPL improvements) and individual developers identity-locked
 *   into exit-conflict between their role as opensource participants and
 *   their desire to commercialize derivatives. The claimed type is
 *   tangled_rope because the constraint coordinates upstream
 *   knowledge-sharing AND extracts from those seeking to exit the commons
 *   institution.
 *
 * KEY AGENTS:
 *   - commons_as_institution: the collectively-governed pool of GPL code and derivative works — benefits from reciprocity enforcement but is not an actor
 *   - proprietary_exit_maximizers: firms excluded from privatizing GPL code (high extraction, no choice)
 *   - individual_developers_with_exit_constraints: developers facing dual loyalty (commons participant vs. commercial actor) — medium extraction, identity-locked exit
 *   - downstream_users: beneficiaries of GPL's guarantee that improvements flow back; bear no direct obligation
 *   - license_enforcement_actors: FSF/SFC/community — administer reciprocity obligation detection and pushback
 *   - proprietary_software_industry: excluded party; would benefit from non-reciprocal incorporation but locked out by license terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.62).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.41).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Commons Institution Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "intellectual_property/software_governance/open_source").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e0b97310-4d53-458a-8d1b-1c352304148d').
narrative_ontology:cs_kernel_codification('e0b97310-4d53-458a-8d1b-1c352304148d', formalized).
narrative_ontology:cs_authority_grounding('e0b97310-4d53-458a-8d1b-1c352304148d', lineage).
narrative_ontology:cs_interpretation_layer_present('e0b97310-4d53-458a-8d1b-1c352304148d').
narrative_ontology:cs_reading_relation('e0b97310-4d53-458a-8d1b-1c352304148d', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0b97310-4d53-458a-8d1b-1c352304148d', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('e0b97310-4d53-458a-8d1b-1c352304148d', foundational, commons_sustainability_through_collective_reciprocity).
narrative_ontology:cs_axiom_status(commons_sustainability_through_collective_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('e0b97310-4d53-458a-8d1b-1c352304148d', commons_sustainability_through_collective_reciprocity, conventional).
narrative_ontology:cs_axiom('e0b97310-4d53-458a-8d1b-1c352304148d', foundational, enclosure_prevention_via_mandatory_forward_sharing).
narrative_ontology:cs_axiom_status(enclosure_prevention_via_mandatory_forward_sharing, holdable).
narrative_ontology:cs_axiom_grounding('e0b97310-4d53-458a-8d1b-1c352304148d', enclosure_prevention_via_mandatory_forward_sharing, deontological).
narrative_ontology:cs_reference_frame('e0b97310-4d53-458a-8d1b-1c352304148d', commons_preservation_via_reciprocity).
narrative_ontology:cs_drift_state('e0b97310-4d53-458a-8d1b-1c352304148d', corporate_cloud_services_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0b97310-4d53-458a-8d1b-1c352304148d', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_developers_with_exit_constraints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_developers_with_exit_constraints).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, derivative_work_communities).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, derivative_work_communities).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_sustainability_through_reciprocity).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, collective_action_problem_solution_via_licensing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collectively-governed pool of GPL code and derivative works that persists and expands because the copyleft obligation forces all improvements back into shared pool. The institution does not collect rents; it enforces a forward-sharing norm that sustains the common resource. Its existence depends on reciprocity enforcement — if developers and firms could extract improvements without reciprocal obligation, the commons would fragment into private forks.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).

% Corporations and proprietary software firms that would benefit from incorporating GPL code into closed products without reciprocal obligation. They are locked out by the GPL license terms: they can use GPL code only if they reciprocate (share modifications) or they abandon GPL code entirely. Their preferred exit (non-reciprocal proprietary integration) is prohibited. They bear the cost of the reciprocity obligation through this constrained choice set.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_exit_maximizers, payer,
    powerful, biographical, constrained, global).

% Individual open-source developers who want to distribute GPL-licensed code but also want to integrate it into proprietary projects, dual-license work, or commercialize variants. They are identity-locked because their self-concept as both 'open-source developer' and 'independent entrepreneur' is incompatible with GPL reciprocity if they want proprietary exit. The reciprocity obligation is extractive from their commercial autonomy perspective, but they benefit from the commons' improvements and community validation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_developers_with_exit_constraints, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_developers_with_exit_constraints, beneficiary).

% End users and derivative-work developers who benefit from the guarantee that improvements to GPL code remain available to all. They have no direct enforcement burden; the reciprocity obligation operates upstream of them. They can choose GPL or non-GPL software, so their exit options are mobile. They benefit from GPL commons preservation without bearing the reciprocity cost.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Communities that build on GPL code (Linux distributions, GNU toolchain variants, embedded derivatives). They both benefit from upstream reciprocity (they inherit all improvements) and bear the reciprocity cost (they must release their modifications). Their constraint is structural: staying in the GPL ecosystem locks them into forward-sharing. Their power is organized because communities collectively influence upstream decisions, but exit options are constrained because staying in the GPL ecosystem requires reciprocal obligation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, derivative_work_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, derivative_work_communities, payer).

% Software Freedom Conservancy, FSF, and community-driven GPL enforcement mechanisms that monitor for violations and push back against proprietary incorporation of GPL code. They administer the reciprocity obligation by detecting violations (e.g., BusyBox litigation, Cisco violations) and initiating legal or social enforcement. Their power is institutional because enforcement authority is conferred by community recognition, legal standing, and the license terms themselves.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, license_enforcement_actors, agenda_setter,
    institutional, generational, analytical, global).

% The broader proprietary software industry (Microsoft pre-2020s, Apple, Oracle, etc.) that is structurally excluded from the commons institution by the reciprocity requirement. They could participate by accepting reciprocity (open-sourcing their code), but that would require surrendering proprietary-business-model protections and competitive advantage. Their exclusion is enforced by the license terms themselves. They would advocate for non-reciprocal incorporation but are not party to the GPL governing arrangement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_software_industry, excluded,
    powerful, biographical, trapped, global).

% Entities holding patents on software techniques that GPL code might implement. They observe GPL enforcement from outside the licensing regime; GPL's reciprocity does not directly constrain them, but GPL's freedom-of-patent-reuse (GPLv3 clause 11) creates downstream tension with patent holders' licensing strategies. They are analytical observers because they do not participate in the commons or violate the reciprocity obligation, but patent disputes affect GPL code adoption and enforcement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, patent_hold_entities, observer,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commons defection problem: if GPL code could be incorporated into proprietary products without reciprocal obligation, individual developers would have incentive to do so (extract private value from shared code), commons improvements would be privatized, and the shared resource would degrade. Mandatory reciprocity ensures all improvements flow back into the shared pool, sustaining a collectively-governed commons that benefits all downstream users and derivative communities.
% TRANSFER_FUNCTION: Transfers the obligation to share code improvements from optional (gift / goodwill) to mandatory (licensing term enforceable through copyright law). Transfers autonomy from individual exit-maximizers and proprietary firms to the commons institution: proprietary integration strategies are prohibited; derivative works must remain GPL. The transfer moves the power to set licensing terms from individual developers (who could choose proprietary) to the collective commons (which enforces reciprocity as a binding condition).
% ABSENT_VOICES: Proprietary software firms that would use GPL code under non-reciprocal terms are structurally excluded — they are not in the governing conversation at all. Individual developers who want to commercial-integrate GPL code without reciprocity are also effectively excluded (their preferred exit is prohibited). They would advocate for permissive-licensing models (Apache, MIT) without reciprocity but are kept out by the GPL license terms themselves, not by debate or democratic process.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity vanished overnight, individual developers and firms would immediately incorporate GPL code into proprietary products. Improvements would fragment across private forks and proprietary variants. The open-source commons would lose its forward-flow guarantee and would reorganize around permissive or proprietary licensing within months. Linux distributions might fork to Apache/MIT licensing to allow proprietary integration. The Linux kernel and GNU ecosystem would cease to be a unified commons and would become a mix of proprietary derivatives and competing permissive forks.
% FOUNDING_PROBLEM: In the 1980s–1990s, proprietary software companies (AT&T, early IBM, Novell) would incorporate Unix and GNU tools into closed systems without contributing improvements back. The shared software resource was being encased in proprietary wrappers, and the private firms benefited while the commons received no return on innovation. This created a tragedy of the commons: individual incentive to privatize conflicted with collective incentive to sustain the shared resource. Stallman designed GPL reciprocity to lock enclosure out by forcing a choice: participate in the commons with reciprocity, or do not use the code at all.
% FOUNDING_PROBLEM_CORROBORATION: Current enclosure pressures from corporate adoption of open-source (Amazon Web Services offering GPL Linux and MySQL as services without releasing internal modifications, as of 2019–2024; Microsoft's acquisition and integration of GPL code into Azure; Google's Linux integration in Android with proprietary services) demonstrate that the founding problem is not solved, only held at bay by enforcement. Independent analysis from Software Freedom Conservancy (GPL enforcement reports), O'Mahony et al. (open-source governance research), and Stallman himself confirm ongoing enclosure attempts and the ongoing need for reciprocity enforcement.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.62 at interval end, plateau after t=20) because the constraint imposes mandatory sharing on developers who might otherwise privatize, but the sharing requirement is reciprocal — participants benefit from others' sharing too. The trajectory shows rising extractiveness from t=0 to t=20 as enforcement mechanisms matured and corporate enclosure attempts increased (cloud-services-without-release-clause era, 2019–2024), then plateaus when enforcement reached institutional stability. Theater is low (0.18) because the reciprocity obligation is materially enforced through legal action and community pushback, not performatively maintained. Suppression is moderate (0.41) because the constraint operates through licensing terms that make violation detectable and actionable, but does not require active surveillance or coercion over willing participants — exit is constrained (can't use GPL code without reciprocity) rather than suppressed (not prevented by force once the choice is understood). Accessibility_collapse at 0.72 reflects that alternatives to GPL exist (Apache, MIT, BSD permissive licenses; proprietary licensing) but they are not true alternatives if one wants to participate in the GPL commons — once the commons is chosen, the reciprocity obligation collapses other paths. Resistance at 0.58 reflects ongoing corporate pushback and developer dissatisfaction with reciprocity (e.g., dual-licensing practices, GPLv2 fork stalemate, cloud-services loopholes), though widespread acceptance of copyleft norm has increased since the 1990s.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary-exit-maximizer seat experiences this as pure constraint (high d toward target, no participation benefit — extraction is enforced absence of their preferred exit). The commons-institution seat (collective beneficiary) experiences it as coordination mechanism (participants get improvement guarantee in exchange for reciprocal obligation). Individual developers sit between: they want commons benefits but also want exit optionality, so they experience asymmetric extraction (they give up individual autonomy to stay in commons). The engine computes these three seats differently from the same structural data: power (organized for commons, powerful for proprietary firms, moderate for individual developers), exit_options (analytical for commons, constrained for firms, identity_locked for developers), and role (beneficiary for commons/downstream, payer for proprietary/individual). This reading REQUIRES that divergence — a commons reading sees the same GPL rule as beneficial institutional coordination from the commons seat and as justified extraction from the exit-maximizer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary = commons_as_institution: the structural arrangement that gains predictability and renewal through reciprocity obligation. Exit option is analytical (not an actor) so directionality derivation yields low d (beneficiary direction). Victims = proprietary_exit_maximizers and individual_developers_with_exit_constraints: the former are locked out by license terms (powerful, trapped exit), the latter are locked into commons by identity-fusion (moderate power, identity_locked exit). Both derive high d (target direction) because they bear extraction cost (mandatory sharing / prohibited privatization) without proportional benefit in their own frameworks. The commons-institution beneficiary anchors the reading: if commons-as-institution is not authored as beneficiary, the constraint collapses into the restriction_reading (focusing only on business-model limits) or the freedom_reading (focusing only on user autonomy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of commons code) is live and continuing (cloud-services clause violations, corporate forks, GPLv3 adoption delays show ongoing enclosure pressure). The constraint persists not because the problem is solved but because GPL enforcement and community norm-maintenance hold it at bay. Mandatrophy would occur if: (a) the founding problem became irrelevant (if code enclosure ceased being economically attractive, which has not happened); or (b) the enforcement apparatus failed and proprietary incorporation became widespread without reciprocal obligation (which has not happened, though the cloud-services loophole came close). The constraint is NOT mandatrophic — it remains live because its function (commons preservation through reciprocal enforcement) is still contested and still functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_as_actor_ambiguity,
    'Is the ''commons-as-institution'' a beneficiary that collects value, or is it a coordination mechanism with no independent interest?',
    'Examine whether GPL communities make decisions about commons sustainability vs. individual developer preference — if communities enforce reciprocity even when developers want to exit, the commons operates as an actor with interests. If reciprocity is merely a sum of individual preferences, the commons is not an independent beneficiary.',
    'If the commons is an independent actor, the constraint is Tangled Rope with legitimate coordination + extraction. If it is merely a coordination mechanism, extraction is one-directional (proprietary firms + exit-maximizers bear cost, downstream users benefit) and the type classifies differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_as_actor_ambiguity, conceptual, 'Whether the commons has independent institutional interests or emerges from individual preferences.').

omega_variable(
    reciprocity_sustainability_boundaries,
    'How much enclosure pressure can reciprocity enforcement absorb before the commons fragments or major participants fork to permissive licenses?',
    'Monitor adoption of permissive alternatives (Apache relicensing, GPLv2 stagnation, fork prevalence) and corporate pushback (cloud-services loopholes, patent threats, dual-licensing prevalence). If majority of new projects choose permissive licenses or if major GPL projects adopt permissive alternatives, the reciprocity institution is losing enforceability.',
    'If enforceability degrades, the constraint''s extractiveness and suppression would rise (coercion needed to maintain a failing norm) while accessibility_collapse would fall (permissive alternatives become real options, not just nominal alternatives). Type could shift toward Piton (performance maintenance) or Snare (suppression-dependent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_sustainability_boundaries, empirical, 'The breaking point of reciprocity as a sustainable commons mechanism.').

omega_variable(
    individual_developer_exit_lock_mechanism,
    'Is individual-developer exit-lock (identity_locked) structural/external (GPL code licenses only on copyleft terms, can''t remix) or internalized (developers have internalized the norm that proprietary integration is ethically wrong)?',
    'Post-exit trajectory: if developers who leave the GPL commons maintain rejection of proprietary integration (e.g., they fork to permissive but still resist commercial use), lock is partially internalized. If they immediately dual-license or proprietary-integrate after leaving, lock was mainly structural.',
    'If lock is internalized, the constraint''s effective suppression is higher than the metric suggests — developers carry the norm with them. If lock is purely structural, a permissive-license exit fully releases the developer and the constraint has no ongoing effect on them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_developer_exit_lock_mechanism, empirical, 'Whether developer exit-lock is structural or internalized.').

omega_variable(
    commons_reading_vs_freedom_reading_foreclosure,
    'Does the commons reading logically foreclose the freedom reading, or can both be held simultaneously?',
    'Test whether a party can hold both: ''GPL sustains a commons AND GPL protects individual user freedoms'' — if yes, they coexist. If a party argues ''commons sustainability requires abandoning individual-freedom emphasis'', that is foreclosure.',
    'If coexistence: the readings coexist_with relation (different parties hold different emphasis). If foreclosure: one reading''s legitimacy claim logically eliminates the other''s core premise, and the cs_structure.reading_relations revises to forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_reading_vs_freedom_reading_foreclosure, conceptual, 'Whether commons and freedom readings are logically incompatible or held simultaneously by different parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.18).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_patent_licensing_compatibility).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_cloud_services_code_capture).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation kernel is decomposed into three constraint stories: (1) copyleft_as_commons_reading (this story) — GPL as commons-sustaining institutional technology; (2) copyleft_as_freedom_reading — GPL as user-freedom-preserving mechanism; (3) copyleft_as_restriction_reading — GPL as business-model constraint. Each story instantiates different beneficiary/victim structures, ε values, and classifications from the same kernel (mandatory code-sharing requirement). They are linked via network.affects_constraints and represent the three live readings of the copyleft mechanism in open-source/software-freedom discourse. The commons reading emphasizes institutional sustainability and collective action against enclosure; the freedom reading emphasizes individual user rights; the restriction reading emphasizes business-model limitation. No single story captures the kernel — all three are necessary to model the constraint complex.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
