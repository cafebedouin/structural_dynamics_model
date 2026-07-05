% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Death Spiral
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the DEMOGRAPHIC TRAP reading of the Lycurgan-laws
 *   kernel: the rhetra's declared unrevisability is taken at face value as
 *   genuinely binding (not a covert-adaptation fiction), and its consequence
 *   — chronic inability to reallocate kleroi, relieve debt, or broaden
 *   citizenship as land concentrated and heirs failed — is read as the
 *   mechanical cause of Spartiate oliganthropia across the 5th-4th centuries
 *   BCE. The classification is snare: a coordination story (equal
 *   citizen-soldiers, stable hoplite class) provided cover for a structure
 *   that, once frozen, transferred political survival away from the future
 *   polity and toward whichever incumbents held land at any given moment,
 *   with the krypteia/helot-suppression apparatus and constitutional appeal
 *   to Lycurgus's authority as the active enforcement. This reading treats
 *   the numbers (roughly 8,000-10,000 Spartiates at Plataea 479 BCE to under
 *   1,000 by Leuctra 371 BCE) as observed drift rather than contested
 *   interpretation.
 *
 * KEY AGENTS:
 *   - homoioi_incumbent_class: agenda_setter/beneficiary (institutional/arbitrage) — controls gerousia/ephorate, defends unrevisability
 *   - impoverished_spartiates: payer (moderate/trapped) — demoted to hypomeiones when kleroi fail
 *   - hypomeiones: payer (powerless/trapped) — bear military cost without political voice
 *   - female_heirs_excluded_from_kleros_reform: excluded (powerless/trapped) — hold land the system needs redistributed but cannot authorize it
 *   - future_spartan_polity: payer (powerless/trapped, civilizational horizon) — inherits the hollowed citizen army
 *   - helot_population: excluded (powerless/trapped) — permanent unfree status is part of the frozen order
 *   - constitutional_theorists_and_historians: observer (analytical) — Aristotle's Politics Book II as founding critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.71).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.8).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Death Spiral").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, 'ce94682a-f74d-448c-89b3-d1cdf102e67c').
narrative_ontology:cs_kernel_codification('ce94682a-f74d-448c-89b3-d1cdf102e67c', fixed_text).
narrative_ontology:cs_authority_grounding('ce94682a-f74d-448c-89b3-d1cdf102e67c', lineage).
narrative_ontology:cs_interpretation_layer_present('ce94682a-f74d-448c-89b3-d1cdf102e67c').
narrative_ontology:cs_reading_relation('ce94682a-f74d-448c-89b3-d1cdf102e67c', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce94682a-f74d-448c-89b3-d1cdf102e67c', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('ce94682a-f74d-448c-89b3-d1cdf102e67c', foundational, formal_unrevisability_was_actually_binding).
narrative_ontology:cs_axiom_status(formal_unrevisability_was_actually_binding, holdable).
narrative_ontology:cs_axiom_grounding('ce94682a-f74d-448c-89b3-d1cdf102e67c', formal_unrevisability_was_actually_binding, empirically_contingent).
narrative_ontology:cs_axiom('ce94682a-f74d-448c-89b3-d1cdf102e67c', foundational, rigidity_is_structural_cause_of_collapse).
narrative_ontology:cs_axiom_status(rigidity_is_structural_cause_of_collapse, holdable).
narrative_ontology:cs_axiom_grounding('ce94682a-f74d-448c-89b3-d1cdf102e67c', rigidity_is_structural_cause_of_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('ce94682a-f74d-448c-89b3-d1cdf102e67c', founding_rhetra_equal_kleros_settlement).
narrative_ontology:cs_drift_state('ce94682a-f74d-448c-89b3-d1cdf102e67c', post_leuctra_oliganthropia_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ce94682a-f74d-448c-89b3-d1cdf102e67c', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_class).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, impoverished_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeiones).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, female_heirs_excluded_from_kleros_reform).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, future_spartan_polity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The full-citizen 'Equals' who control the gerousia and ephorate and treat the rhetra's unrevisability as sacred, refusing land redistribution or citizenship broadening even as their own numbers shrink. Their status depends on the kleros allotment system remaining formally unchanged; they administer the enforcement that keeps it frozen and are the last group to feel the collapse because they hold the concentrated remaining land.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_class, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_class, beneficiary).

% Spartiates who lose or cannot maintain their kleros through debt, partition among heirs, or battlefield loss of the male line, and are consequently demoted from full citizenship (becoming hypomeiones) because the law makes no provision for kleros reallocation or debt relief. They bear the direct cost of the system's rigidity: loss of political personhood despite Spartan ancestry.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, impoverished_spartiates, payer,
    moderate, biographical, trapped, local).

% Former citizens stripped of full rights by economic failure under the frozen allotment system. They fight in Spartan armies and bear the costs of the citizen-body's demographic contraction without political voice to reform the very rules that demoted them; no legal path exists to restore standing.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeiones, payer,
    powerless, biographical, trapped, local).

% Women who accumulate substantial land through inheritance and dowry consolidation (documented by Aristotle as controlling up to two-fifths of Spartan territory) but have no formal role in the political system that could authorize redistributing that land to reverse citizen decline. Their structural position — holding land the system needs redistributed but excluded from the assembly that could do it — is a symptom of the rhetra's inflexibility, not a cause outside it.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, female_heirs_excluded_from_kleros_reform, excluded,
    powerless, generational, trapped, local).

% The political community's own long-term survival, which pays the compounding cost of a citizen body contracting from an estimated 8,000-10,000 at Plataea to under 1,000 by Leuctra. It has no seat at any table because it does not yet exist at the moment the rigid rules are defended; by the time the shortfall (oliganthropia) is undeniable, the army cannot be fielded and the reform comes too late (Agis IV's attempted redistribution, generations afterward, fails against entrenched resistance).
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, future_spartan_polity, payer,
    powerless, civilizational, trapped, regional).

% The subjugated agricultural labor force whose permanent unfree status is itself part of the frozen constitutional order (the krypteia and helot suppression apparatus). They would be an obvious source of manpower and social flexibility if the system could adapt citizenship criteria, but the rhetra's rigidity forecloses this along with every other adaptive path; they are outside the conversation entirely, their subjugation treated as a fixed premise rather than a policy choice.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_population, excluded,
    powerless, generational, trapped, local).

% Ancient and modern analysts (Aristotle's Politics Book II being the founding critique) who examine the rhetra's land and citizenship provisions as a case study in how formal unrevisability, defended as timeless wisdom, produced a mechanically traceable population collapse over roughly two centuries.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, constitutional_theorists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_class).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original kleros allotment and rhetra system solved a genuine problem: preventing land concentration and internal factionalism among the Spartiate warrior class by guaranteeing every citizen-soldier a subsistence estate worked by helots, freeing him for military training. This coordinated a stable, materially-equal hoplite citizenry.
% TRANSFER_FUNCTION: Over time the arrangement's rigidity transferred political status away from Spartiates whose kleroi failed (through debt, partition, or battlefield loss of heirs) toward the shrinking core who retained land, and transferred survival risk from the present incumbent generation onto the future polity, which inherited a hollowed-out citizen army it could not reconstitute.
% ABSENT_VOICES: Female heirs holding accumulating land, hypomeiones stripped of citizenship, and above all the future citizen-generations who would need the reform are structurally absent from the assembly that could revise the rhetra — by the time the shortfall was undeniable (post-Leuctra, 371 BCE), reformers like Agis IV faced land-holding elites for whom the frozen system was still, individually, advantageous.
% DISAPPEARANCE_RATIONALE: Had the unrevisability norm not held — had kleros reallocation, debt cancellation, or citizenship broadening been legally available reform paths as pressures mounted in the 5th-4th centuries BCE — the Spartiate citizen body's contraction from roughly 8,000-10,000 (479 BCE) to under 1,000 (371 BCE) was not the only possible outcome; the polity's military and political capacity, and its eventual eclipse by Thebes, plausibly follow a materially different path.
% FOUNDING_PROBLEM: Archaic Sparta faced land disputes and stasis risk among its warrior elite (Second Messenian War era); the rhetra attributed to Lycurgus fixed land allotment, military training (agoge), and communal messes (syssitia) as an unchangeable constitutional settlement to prevent factional collapse and guarantee a permanent hoplite class.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle, writing from outside the Spartan citizen class and with access to comparative constitutional data (Politics, Book II), explicitly identifies the land-law's failure to prevent concentration and the exclusion of women's landholding from reform as structural defects causing oliganthropia — a corroboration from an analytical outsider, not from the homoioi who continued to defend the rhetra's sanctity even as the citizen rolls emptied.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.71 over the interval on a single shared grid, tracking increasing land concentration among fewer surviving lineages as kleroi failed and were absorbed rather than redistributed. Suppression is authored high throughout (0.5 rising to 0.8) because the rhetra's unrevisability was defended by real constitutional and religious sanction from the outset, not merely invoked once crisis emerged — the krypteia and syssitia exclusion mechanisms for failed citizens were structural from early on. Theater ratio rises moderately (0.15 to 0.4) as the gap widened between the rhetra's professed function (guaranteeing a stable equal citizenry) and its actual operation (a shrinking oligarchy defending land title) — by the 4th century, invoking Lycurgus's authority was increasingly performative relative to the system's collapsing substantive function. Accessibility collapse (0.62) reflects that once citizens understood the kleros-and-birth criteria for Spartiate status, no informal workaround existed; resistance (0.55) reflects real internal reform attempts (Agis IV, Cleomenes III, generations later) that were suppressed by the incumbent land-holding faction.
 *
 * DIRECTIONALITY LOGIC:
 *   The homoioi incumbent class sits nearest full beneficiary: their land title and political monopoly are underwritten precisely by the freeze the rhetra imposes, and their exit option is effectively arbitrage — they can absorb failed neighbors' land without themselves bearing citizenship risk. Impoverished Spartiates and hypomeiones sit near full target: trapped by birth-based identity (Spartiate status is not portable — losing it is not exit, it is demotion within the only polity that defines you), they bear the constraint's cost directly and cannot buy their way out. Female heirs and helots are excluded rather than coordinated: they hold resources or capacity the system needs but structurally cannot access the mechanism that would let them contribute to reform. The future polity is the most severe case of directional asymmetry — it bears near the totality of the accumulated cost (the demographic floor that could not field an army at Leuctra) while having zero voice in any generation that could have amended the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing land-based factionalism among a small hoplite elite) was genuinely live in archaic Sparta and the rhetra's rigidity was a defensible, even effective, solution at founding. Its status shifts to dead well before the visible collapse: by the mid-5th century, kleros failure was routine and the coordination function (guaranteeing every citizen a subsistence estate) had already broken down into land concentration, while the SUPPRESSION apparatus (constitutional sanctity, krypteia, exclusion of hypomeiones from political process) remained fully intact and arguably intensified. This is the mandatrophy signature precisely: the mandate (equal citizen-soldiers) died while the enforcement of the mandate's letter (unrevisable land/citizenship law) not only persisted but hardened, because the incumbents who benefited from land concentration were also the only body empowered to authorize revision. The classification prevents mislabeling this as pure natural decline (a mountain) or as functioning coordination (a rope) — it names the structure as a snare precisely because a real coordination story existed at founding and was retained as cover for what became pure extraction of political durability from the future.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rigidity_vs_covert_adaptation,
    'Did the Spartan system actually enforce kleros/citizenship rules as rigidly as the rhetra claimed, or did informal adaptation (debt forgiveness, quiet land transfers, flexible enforcement of citizenship criteria) occur beneath the formally unrevisable surface, such that the demographic collapse had causes other than pure constitutional rigidity?',
    'Comparative reading of Aristotle''s Politics Book II against Plutarch''s Lycurgus and epigraphic/archaeological evidence for actual land transaction patterns in Laconia across the 5th-4th centuries BCE; convergence of independent ancient sources on rigid enforcement would support this reading, while evidence of informal flexibility would support the sibling adaptive_fiction_reading instead.',
    'If covert adaptation is established as the dominant mechanism, this constraint (demographic_trap_reading) is the WRONG reading of the kernel and adaptive_fiction_reading becomes the structurally accurate account — the two are not compatible descriptions of the same events, they are rival causal claims about the same historical record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_vs_covert_adaptation, empirical, 'Whether the rhetra''s formal rigidity was the actual operative cause of demographic collapse, as this reading claims, or whether informal adaptation occurred beneath a rigid formal surface.').

omega_variable(
    constructed_vs_natural_decline,
    'Is Spartiate demographic collapse better modeled as a constructed outcome of a specific, revisable legal architecture (kleros allotment + citizenship-by-birth-and-land), or as a comparatively natural consequence of any small closed elite''s exposure to war losses, given ancient warfare''s inherent attrition on any sufficiently narrow citizen class?',
    'Comparative case study against other ancient poleis with similarly narrow hoplite-citizen classes but different land/inheritance law (e.g. Athenian citizenship reforms, other Dorian poleis) to isolate whether legal rigidity specifically, versus generic warfare attrition, better predicts the rate and shape of the decline.',
    'If the decline pattern matches generic small-elite war attrition regardless of legal architecture, the snare classification is overstated and the constraint is closer to a tragic but non-extractive structural limit; if the decline pattern is specific to Sparta''s inheritance/kleros rigidity relative to comparably-sized elites elsewhere, the snare reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_decline, empirical, 'Whether legal unrevisability specifically, rather than generic elite-attrition dynamics, drove the demographic outcome.').

omega_variable(
    beneficiary_class_stability,
    'Did the homoioi incumbent class actually benefit net-net from the frozen system across the full two-century interval, or did the eventual military weakness (culminating in defeat at Leuctra and loss of Messenian helot labor) impose costs on the incumbent class itself that erode the clean beneficiary/victim split this reading assumes?',
    'Track land concentration data and military outcomes together: if the surviving homoioi lineage at 371 BCE held proportionally more land and status than their 479 BCE counterparts despite Sparta''s overall military collapse, the beneficiary framing holds at the individual/lineage level even as the polity as a whole loses.',
    'If incumbents also lost heavily in the final collapse (loss of Messenia removed the helot labor base underwriting even surviving kleroi), the extraction may be better modeled as a collective-action failure with delayed universal cost rather than a clean incumbent-beneficiary structure — this would push the classification toward tangled_rope rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_stability, conceptual, 'Whether the incumbent beneficiary class was insulated from the eventual collapse or ultimately shared in its costs, affecting the snare-vs-tangled_rope boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(lycu_tr_t0, observed).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__demographic_trap_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(lycu_tr_t40, observed).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__demographic_trap_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(lycu_tr_t80, observed).
narrative_ontology:measurement(lycu_tr_t120, lycurgan_laws__demographic_trap_reading, theater_ratio, 120, 0.33).
narrative_ontology:measurement_basis(lycu_tr_t120, observed).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__demographic_trap_reading, theater_ratio, 160, 0.37).
narrative_ontology:measurement_basis(lycu_tr_t160, observed).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.39).
narrative_ontology:measurement_basis(lycu_tr_t200, observed).
narrative_ontology:measurement(lycu_tr_t220, lycurgan_laws__demographic_trap_reading, theater_ratio, 220, 0.4).
narrative_ontology:measurement_basis(lycu_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(lycu_be_t0, observed).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__demographic_trap_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(lycu_be_t40, observed).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__demographic_trap_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement_basis(lycu_be_t80, observed).
narrative_ontology:measurement(lycu_be_t120, lycurgan_laws__demographic_trap_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement_basis(lycu_be_t120, observed).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__demographic_trap_reading, base_extractiveness, 160, 0.65).
narrative_ontology:measurement_basis(lycu_be_t160, observed).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.7).
narrative_ontology:measurement_basis(lycu_be_t200, observed).
narrative_ontology:measurement(lycu_be_t220, lycurgan_laws__demographic_trap_reading, base_extractiveness, 220, 0.71).
narrative_ontology:measurement_basis(lycu_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(lycu_su_t0, observed).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__demographic_trap_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(lycu_su_t40, observed).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__demographic_trap_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement_basis(lycu_su_t80, observed).
narrative_ontology:measurement(lycu_su_t120, lycurgan_laws__demographic_trap_reading, suppression_requirement, 120, 0.71).
narrative_ontology:measurement_basis(lycu_su_t120, observed).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__demographic_trap_reading, suppression_requirement, 160, 0.76).
narrative_ontology:measurement_basis(lycu_su_t160, observed).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.79).
narrative_ontology:measurement_basis(lycu_su_t200, observed).
narrative_ontology:measurement(lycu_su_t220, lycurgan_laws__demographic_trap_reading, suppression_requirement, 220, 0.8).
narrative_ontology:measurement_basis(lycu_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the lycurgan_laws kernel, each instantiating a structurally distinct claim about the SAME fixed text (the rhetra attributed to Lycurgus) rather than a measurement-parameter variation of one claim, per the epsilon-invariance decomposition principle. sacral_fidelity_reading treats the unrevisability as legitimate divine ordinance (near-mountain from the internal Spartan religious frame). adaptive_fiction_reading treats the apparent rigidity as a noble lie masking covert adaptive practice (closer to rope or tangled_rope, since the real operative rule differs from the professed rule). demographic_trap_reading (this file) treats the formal rigidity as genuinely operative and causally responsible for the documented Spartiate population collapse, warranting snare classification because a real founding coordination function was retained as cover for an outcome that primarily served incumbent land-holders at the future polity's expense. The three readings are mutually exclusive causal claims about the same historical record, not compatible perspectives on one event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
