% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Catastrophe-Memory Survival — Competence Transmission Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A cross-generational arrangement in catastrophe-exposed and diasporic
 *   communities packages practical survival knowledge — planting and travel
 *   timing, food safety and preservation rules, resource-pooling obligations,
 *   mutual-aid and hospitality protocols, family-formation strategies — into
 *   ritually enforced practice, so that the knowledge survives expulsion,
 *   library destruction, and institutional collapse and remains available to
 *   mobile populations. Under the competence-transmission reading, the
 *   standing arrangement is assessed by whether the encoded knowledge arrives
 *   usable at the point of need: where decoding chains hold, the arrangement
 *   works as a high-overhead but catastrophe-resistant curriculum; where they
 *   break, communities keep paying observance costs for content they can no
 *   longer retrieve. Per the epsilon-invariance decomposition, the colloquial
 *   label 'ritual preserves communities through catastrophe' splits into
 *   structurally distinct claims; this file authors only the
 *   practical-content claim and links its siblings via
 *   network.affects_constraints. The epsilon referent is the standing ritual
 *   arrangement as this reading assesses it — never the explicit-instruction
 *   alternative this reading might prefer. KEY AGENTS (by structural
 *   relationship): - religious_authority_structures: agenda-setter and
 *   principal beneficiary (institutional/arbitrage) — administers observance
 *   standards, collects status and support - decoding_diaspora_communities:
 *   primary beneficiary (organized/mobile) — deploys encoded knowledge across
 *   displacements - form_maintaining_assimilating_communities: primary target
 *   (moderate/identity_locked) — bears observance costs with degraded content
 *   access - functioning_observant_households: near-symmetric participant
 *   (moderate/constrained) — costs and benefits locally matched -
 *   young_generation_ritual_learners: deferred-cost bearer
 *   (powerless/constrained) — supplies rehearsal labor before any payoff -
 *   secular_knowledge_institutions: excluded competitor (powerful/trapped) —
 *   barred from the governed domains - comparative_ritual_scholars:
 *   analytical observer (analytical/analytical) — sees the full encoding
 *   structure
 *
 * KEY AGENTS:
 *   - religious_authority_structures: agenda-setter and principal beneficiary (institutional/arbitrage) — administers standards, collects status and support
 *   - decoding_diaspora_communities: primary beneficiary (organized/mobile) — carries and applies the encoded toolkit across host societies
 *   - form_maintaining_assimilating_communities: primary target (moderate/identity_locked) — pays full observance costs for degraded content
 *   - functioning_observant_households: near-symmetric participant (moderate/constrained) — felt benefits roughly match felt costs
 *   - young_generation_ritual_learners: deferred-cost bearer (powerless/constrained) — rehearsal labor now, contingent payoff later
 *   - secular_knowledge_institutions: excluded competitor (powerful/trapped) — would transmit the content explicitly, kept outside the frame
 *   - comparative_ritual_scholars: analytical observer (analytical/analytical) — maps which practices encode which knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Catastrophe-Memory Survival — Competence Transmission Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'dd9e73b2-b204-4b95-8a1f-e46075207e15').
narrative_ontology:cs_kernel_codification('dd9e73b2-b204-4b95-8a1f-e46075207e15', distributed).
narrative_ontology:cs_authority_grounding('dd9e73b2-b204-4b95-8a1f-e46075207e15', distributed).
narrative_ontology:cs_reading_relation('dd9e73b2-b204-4b95-8a1f-e46075207e15', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd9e73b2-b204-4b95-8a1f-e46075207e15', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('dd9e73b2-b204-4b95-8a1f-e46075207e15', foundational, ritual_practical_content_is_load_bearing).
narrative_ontology:cs_axiom_status(ritual_practical_content_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('dd9e73b2-b204-4b95-8a1f-e46075207e15', ritual_practical_content_is_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('dd9e73b2-b204-4b95-8a1f-e46075207e15', foundational, encoded_knowledge_decays_without_active_decoding).
narrative_ontology:cs_axiom_status(encoded_knowledge_decays_without_active_decoding, holdable).
narrative_ontology:cs_axiom_grounding('dd9e73b2-b204-4b95-8a1f-e46075207e15', encoded_knowledge_decays_without_active_decoding, empirically_contingent).
narrative_ontology:cs_reference_frame('dd9e73b2-b204-4b95-8a1f-e46075207e15', ritual_as_competence_repository).
narrative_ontology:cs_drift_state('dd9e73b2-b204-4b95-8a1f-e46075207e15', contemporary_explicit_documentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd9e73b2-b204-4b95-8a1f-e46075207e15', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, decoding_diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, religious_authority_structures).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, form_maintaining_assimilating_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, functioning_observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, functioning_observant_households).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, young_generation_ritual_learners).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, embodied_knowledge_persistence_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, ritual_as_information_technology_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordain standards of observance, train the transmitters, adjudicate disputes over application, and decide what counts as correct performance. Collect dues, status, and institutional continuity from administering the system. Can reinterpret content when environments shift, which is also how the gap between stored knowledge and current conditions gets managed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, religious_authority_structures, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, religious_authority_structures, beneficiary).

% Communities dispersed by expulsion, pogrom, or famine that kept portable practice bundles — festival calendars marking safe-travel and planting windows, pooling rules, mutual-aid obligations, hospitality protocols — and deployed them in new host societies. The encoded knowledge travels with them when institutions and libraries do not.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, decoding_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Communities several generations into integration where the decoding chain has thinned. They keep the dietary rules, the calendar, and endogamy expectations as identity markers while the practical heuristics underneath arrive garbled or obsolete. They pay observance costs in time, money, and narrowed marriage and economic networks; exit reads as betrayal of the dead, so leaving feels like self-erasure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, form_maintaining_assimilating_communities, payer,
    moderate, biographical, identity_locked, national).

% Households inside still-coherent communities that keep rest-day, food-rule, and festival obligations. The disciplines still deliver thrift, food safety, and mutual aid they can feel, while the time and income costs are real. Their assessment of the bargain flips with the local quality of the decoded content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, functioning_observant_households, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, functioning_observant_households, payer).

% Children enrolled in years of memorization and rehearsed practice before they can evaluate the content. They supply the rehearsal labor the transmission system runs on; their eventual payoff depends on whether their community still holds decoding capacity when they need it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, young_generation_ritual_learners, payer,
    powerless, biographical, constrained, local).

% Schools, agricultural extensions, public-health bodies, and publishers who could transmit the same survival content explicitly and update it continuously. They sit outside the ritual frame: the domains the frame governs — diet, calendar, family formation — were fenced off from them, and their proposals to replace rote observance with documented instruction are received as assimilation threats rather than competing channels.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, secular_knowledge_institutions, excluded,
    powerful, generational, trapped, global).

% Historians and anthropologists comparing which practices encode which knowledge across traditions and testing content claims against disaster outcomes. They publish decoding keys but command no enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, religious_authority_structures).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Packages hard-won practical knowledge — agricultural and travel timing, food safety and preservation, resource pooling, mutual-aid protocols, family-formation strategies — into memorable, enforceable, catastrophe-resistant practice, and reproduces it across generations without depending on fragile written or institutional channels.
% TRANSFER_FUNCTION: Moves observance costs (time, dietary restriction, endogamy limits, dues, childhood rehearsal labor) from participating households into the maintenance of the transmission system; moves decoded adaptive capacity to whoever retains access to the code — disproportionately to mobile diaspora communities and to the authority structures that interpret it.
% ABSENT_VOICES: Secular knowledge institutions and would-be explicit transmitters are outside the frame, as are members who left and could report which parts of the encoded knowledge failed them. Within-frame dissenters who question whether specific observances still carry content are managed as discipline problems rather than consulted as data sources.
% DISAPPEARANCE_RATIONALE: If the transmission system vanished overnight, diaspora communities would lose a catastrophe-tested portable channel for timing, pooling, and mutual-aid knowledge; authority structures would lose their administrative object; households would have to rebuild food, calendar, and family protocol around explicit institutions — slow, uneven work, weakest exactly where catastrophe risk is highest.
% FOUNDING_PROBLEM: Catastrophe repeatedly destroys the institutions and records that carry practical survival knowledge. Communities needed a channel that survives book-burning, expulsion, and institutional collapse, and that keeps the knowledge available to scattered, mobile populations.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociology and forced-migration research corroborate that displaced communities rely on embodied, ritually reinforced knowledge networks when formal institutions fail; historical studies of famine and expulsion document ritual-carried practices — food rules, pooling obligations, timing disciplines — operating in crises. These attesting sources sit outside the benefiting parties; the authority structures' own testimony of liveness is discounted as self-interested.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate): observance costs — dietary restriction, calendar discipline, endogamy pressure, dues, years of childhood rehearsal — are levied broadly, while decoded access is uneven and decaying content converts cost into pure burden for form-maintaining communities; the transmission payoff for decoding communities keeps epsilon well below snare range. Suppression is authored at 0.48 as a raw structural property, unscaled by power or scope in the engine's arithmetic: communal sanction, religious courts, and educational pressure enforce observance, but exit through assimilation exists at identity cost. Theater ratio 0.27: most maintained practice still performs transmission work; a minority is form-maintenance performance. Accessibility collapse 0.35: once the encoding is seen, explicit alternatives (documentation, schooling, extension services) become visible and partly viable, but they historically failed exactly under catastrophe conditions, so they do not fully substitute. Resistance 0.42: assimilation, reform movements, and secularization are continuous low-grade exits. The three measurement series share one seven-point grid (1900-2020, twenty-year steps); the 1940 row records wartime duress — enforcement and extraction spiking together as visible practice turned lethal — followed by postwar liberalization, then renewed hardening alongside content decay. Gains demonstrably accrue to the authority seat (dues, status, administrative continuity), hence gain_flow names it; fixing is prohibitive for that seat because dismantling enforcement dissolves its own position, and replacement by explicit institutions fails precisely under the catastrophe conditions the arrangement was built for.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical doctrine. Form-maintaining communities (identity_locked exit: leaving reads as betraying the dead — a relational-identity fusion with ancestral practice) experience the arrangement as extraction sustained by their own fidelity; decoding diaspora communities (mobile) experience it as an inherited toolkit; authority structures experience it as an administration they run. Young learners bear costs before any payoff and cannot evaluate the trade. If the identity frame broke — if exit stopped reading as self-erasure — the form-maintainer seat would migrate toward mobile exit and its computed extraction would fall; the divergence is sensitive to identity-lock, not to doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: religious_authority_structures and decoding_diaspora_communities are declared beneficiaries (low d — the arrangement subsidizes them); form_maintaining_assimilating_communities is the declared victim (high d). Three overrides correct relationships the declaration-based derivation cannot see, each keyed to a power atom with a single occupant in this story so no cross-agent contamination occurs: young_generation_ritual_learners (powerless -> 0.68) bear rehearsal costs years before any contingent payoff, an asymmetry invisible to beneficiary/victim data; secular_knowledge_institutions (powerful -> 0.8) are suppressed competitors — exclusion is not expressible as a victim declaration, so the override encodes their targeted position; functioning_observant_households are left to derivation and sit near symmetric (about 0.5), costs and benefits locally matched.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled-rope classification prevents both mislabels: calling the whole arrangement rope denies the form-maintainer victims (extraction denial); calling it snare erases the demonstrated survival payoff for decoding communities (coordination denial). The R5 interview finds the founding problem — carrying survival knowledge across institutional collapse — still live wherever displacement and catastrophe remain common, so no mandatrophy resolution is declared; but the drift series shows the failure mode accumulating: content decodes less reliably while enforcement re-hardens, and if the live_content_vs_form_ratio omega resolves toward form-dominance, the arrangement's mandate will have outlived its function for the affected subpopulations even while remaining live elsewhere — the piton-drift risk this corpus should track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_referent,
    'Which arrangement does this story''s epsilon describe, given that it is one reading of the contested catastrophe_memory_survival kernel?',
    'Cross-file comparison of the three reading stories: the referent is fixed here as the standing ritual arrangement assessed by competence-transmission lights; sibling files author their own epsilon over the same practices.',
    'The symbol reading relocates extraction to identity-enforcement costs with a different victim set; the hybrid reading splits epsilon across registers. Treating any single file''s epsilon as the kernel''s epsilon conflates distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_referent, conceptual, 'Committer-frame omega: one reading, one epsilon, one referent among three sibling readings of the same kernel.').

omega_variable(
    live_content_vs_form_ratio,
    'What fraction of currently maintained ritual practice still transmits usable survival content versus form-only maintenance?',
    'Content-elicitation studies and disaster-response observation: do communities actually deploy the encoded heuristics (timing, pooling, food rules) under stress, or only perform them?',
    'A high form-ratio expands the victim set and pushes the computed type toward snare; a low form-ratio supports the rope-leaning coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(live_content_vs_form_ratio, empirical, 'Whether observed persistence reflects live transmission or hollowed form.').

omega_variable(
    enforcement_necessity_for_retention,
    'Is active enforcement load-bearing for knowledge retention (disciplined rehearsal across generations), or is it extractive overhead riding on the transmission function?',
    'Compare retention and adaptation outcomes across voluntarily practicing versus sanction-enforced communities holding similar content.',
    'If enforcement is necessary, its costs are coordination price and the reading leans rope; if excess, the same structure leans snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_necessity_for_retention, empirical, 'Whether the enforcement apparatus is functional to transmission or parasitic on it.').

omega_variable(
    obsolete_content_share,
    'What share of the encoded content is obsolete under current climates, markets, and health regimes yet still enforced?',
    'Domain-by-domain audit of encoded heuristics against current environmental and epidemiological data.',
    'Obsolete-and-enforced content is pure cost: it raises effective extraction for communities in shifted environments and accelerates the piton-drift risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obsolete_content_share, empirical, 'How much of the stored curriculum still pays in present conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_reading_tr_t1900, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(competence_reading_tr_t1900, observed).
narrative_ontology:measurement(competence_reading_tr_t1920, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1920, 0.17).
narrative_ontology:measurement_basis(competence_reading_tr_t1920, observed).
narrative_ontology:measurement(competence_reading_tr_t1940, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement_basis(competence_reading_tr_t1940, observed).
narrative_ontology:measurement(competence_reading_tr_t1960, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement_basis(competence_reading_tr_t1960, observed).
narrative_ontology:measurement(competence_reading_tr_t1980, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(competence_reading_tr_t1980, observed).
narrative_ontology:measurement(competence_reading_tr_t2000, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(competence_reading_tr_t2000, observed).
narrative_ontology:measurement(competence_reading_tr_t2020, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(competence_reading_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(competence_reading_be_t1900, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1900, 0.36).
narrative_ontology:measurement_basis(competence_reading_be_t1900, observed).
narrative_ontology:measurement(competence_reading_be_t1920, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1920, 0.39).
narrative_ontology:measurement_basis(competence_reading_be_t1920, observed).
narrative_ontology:measurement(competence_reading_be_t1940, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement_basis(competence_reading_be_t1940, observed).
narrative_ontology:measurement(competence_reading_be_t1960, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1960, 0.44).
narrative_ontology:measurement_basis(competence_reading_be_t1960, observed).
narrative_ontology:measurement(competence_reading_be_t1980, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 1980, 0.46).
narrative_ontology:measurement_basis(competence_reading_be_t1980, observed).
narrative_ontology:measurement(competence_reading_be_t2000, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 2000, 0.49).
narrative_ontology:measurement_basis(competence_reading_be_t2000, observed).
narrative_ontology:measurement(competence_reading_be_t2020, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(competence_reading_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(competence_reading_su_t1900, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement_basis(competence_reading_su_t1900, observed).
narrative_ontology:measurement(competence_reading_su_t1920, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1920, 0.74).
narrative_ontology:measurement_basis(competence_reading_su_t1920, observed).
narrative_ontology:measurement(competence_reading_su_t1940, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1940, 0.85).
narrative_ontology:measurement_basis(competence_reading_su_t1940, observed).
narrative_ontology:measurement(competence_reading_su_t1960, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement_basis(competence_reading_su_t1960, observed).
narrative_ontology:measurement(competence_reading_su_t1980, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 1980, 0.34).
narrative_ontology:measurement_basis(competence_reading_su_t1980, observed).
narrative_ontology:measurement(competence_reading_su_t2000, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(competence_reading_su_t2000, observed).
narrative_ontology:measurement(competence_reading_su_t2020, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement_basis(competence_reading_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, information_standard).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual preserves communities through catastrophe' decomposes into three epsilon-invariant claims: this file carries the practical-content claim (moderate epsilon; victims are communities maintaining form without retrievable content; beneficiaries are decoding diaspora communities and the interpreting authority). The symbol_survival_reading carries the identity-continuity claim and the hybrid_encoding_reading the conjunction claim; each warrants its own epsilon, metrics, and stakeholders. The symbol reading functions as the folk baseline; competence findings feed the hybrid reading's plausibility, which is why this file declares an influence edge to it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, powerless, 0.68).
constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
