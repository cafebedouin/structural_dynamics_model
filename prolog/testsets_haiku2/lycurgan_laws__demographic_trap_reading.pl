% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Lycurgan laws in Sparta represent a constitutional order claimed to be
 *   unchangeable—either because Lycurgus himself received them from Apollo
 *   (sacral reading) or because they were encoded as unchangeable by design
 *   (this demographic-trap reading). This story instantiates the
 *   demographic-trap reading: the laws' immutability is not a feature of
 *   sacred origin but a structural mechanism that prevents adaptation to
 *   changed conditions. The system solved Sparta's founding problem (internal
 *   conflict and helot revolt) by creating radical equality and communal
 *   discipline among Spartiates, enforced by unchangeable law. Over
 *   centuries, that rigidity became a death sentence: strict marriage laws
 *   and the requirement that younger sons leave or remain landless caused
 *   population collapse. The laws that once preserved Sparta by preventing
 *   revision now prevent Sparta from escaping demographic suicide. The
 *   constraint is classified as snare because the immutability operates as a
 *   suppression mechanism that forecloses alternatives (adaptation through
 *   revision) while extracting from future generations and the powerless.
 *
 * KEY AGENTS:
 *   - Ephorate and Gerusian Council: institutional agenda-setter, claims to be custodian of unchangeable law; derives authority from immutability itself
 *   - Old Spartiate families: beneficiaries of unchangeable property and citizenship laws; entrenched elite protected by immutability from pressure to distribute wealth or opportunity
 *   - Younger Spartiates: victims trapped by unchangeable inheritance law that allows only eldest heir; face economic marginalization and forced exit (mercenary service or loss of citizenship)
 *   - Non-Spartiate inhabitants (helots, perioikoi): structural victims bearing labor extraction and class subjugation under unchangeable law; cannot revise
 *   - Future generations: victims of a system whose rigidity prevents adaptation to population decline; inherit a shrinking citizen base and no institutional path to reform
 *   - Constitutional reformers (Agis IV, Cleomenes III): excluded by frame; any reform proposal is suppressed as impiety; identity-locked (cannot leave without renouncing Spartiate identity)
 *   - Priesthood: collude with ephorate to legitimize immutability via divine origin narrative; benefit from interpretive monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.89).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political/constitutional").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '3c9c3d1e-e68c-4a89-ac53-8702a81a7924').
narrative_ontology:cs_kernel_codification('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', fixed_text).
narrative_ontology:cs_authority_grounding('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', extraction).
narrative_ontology:cs_interpretation_layer_present('3c9c3d1e-e68c-4a89-ac53-8702a81a7924').
narrative_ontology:cs_reading_relation('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', foundational, immutability_is_real_constraint).
narrative_ontology:cs_axiom_status(immutability_is_real_constraint, holdable).
narrative_ontology:cs_axiom_grounding('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', immutability_is_real_constraint, empirically_contingent).
narrative_ontology:cs_axiom('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', foundational, unchangeable_laws_cause_demographic_trap).
narrative_ontology:cs_axiom_status(unchangeable_laws_cause_demographic_trap, holdable).
narrative_ontology:cs_axiom_grounding('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', unchangeable_laws_cause_demographic_trap, empirically_contingent).
narrative_ontology:cs_reference_frame('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', lycurgan_immutable_order).
narrative_ontology:cs_drift_state('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', fourth_century_collapse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3c9c3d1e-e68c-4a89-ac53-8702a81a7924', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, old_spartiate_families).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, younger_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, non_spartiate_inhabitants).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, priesthood_and_oracles).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, future_spartiate_generations).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, constitutional_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, divine_origin_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets Lycurgan law, controls the agora assembly, enforces citizen discipline, and manages helot labor allocation. Claims to be custodians of Lycurgus's constitutional order, unchangeable by decree. Their authority rests on the immutability claim itself; any revision would undermine their custodial role. They interpret existing law creatively in practice but cannot formally revise it without delegitimizing the entire framework.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_and_gerusian_council, agenda_setter,
    institutional, generational, mobile, local).

% Hold consolidated land and political position under the unchanging laws. They benefit from inheritance stability, exclusion of new families through unchangeable citizenship restrictions, and the frozen social hierarchy that protects their standing. The immutability of property law protects their accumulated wealth; the inability to revise kinship and land rules preserves their dominance. They have practical exit via mercenary service but are socially invested in the Spartan order itself.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, old_spartiate_families, beneficiary,
    powerful, biographical, mobile, local).

% Face the consequences of unchangeable land laws that prevent property subdivision among brothers: the law allows only one heir per household to prevent fragmentation. Younger sons cannot inherit, cannot acquire new land, and are economically marginalized despite full citizenship. They are locked into quasi-military careers or mercenary service. The unchangeability of the law means no revision to make inheritance more flexible can occur; their exit option is to leave Sparta entirely, which means loss of citizenship and identity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, younger_spartiates, payer,
    moderate, biographical, constrained, local).

% Includes perioikoi (free non-citizens) and helots (enslaved population). They bear the coercive labor burden that Lycurgan laws enable: the unchangeable military communalism of Spartiates rests on confiscated labor. They cannot petition for revision of the laws that enslave or subordinate them; the immutability doctrine extends to the class system itself. Their only exit is violent revolt or flight, both extremely costly.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, non_spartiate_inhabitants, payer,
    powerless, immediate, trapped, local).

% Are locked into a system designed by Lycurgus circa 600 BCE with no institutional path to revise it for changed demographic, military, or economic conditions. As population declines (due to unchangeable high-risk military expectation and strict marriage rules preventing reproduction in younger sons), the system cannot adapt. They inherit a shrinking citizen base and a constitution that forbids the adaptation that would save it.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, future_spartiate_generations, payer,
    powerless, generational, trapped, local).

% Would benefit strategically if Sparta's constitutional rigidity causes it to collapse. They observe the demographic trap but have no seat at the table to advocate for Spartan adaptive reform. Their exclusion is automatic—foreign powers do not participate in the agora—but their strategic observation of Spartan inflexibility informs their own military planning.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, external_military_rivals, excluded,
    institutional, generational, analytical, regional).

% Exist among younger Spartiates and some moderates in the gerusian council who recognize the unsustainability of the laws (e.g., Agis IV, Cleomenes III in historical Sparta). They are trapped: they understand the trap but cannot revise the constitution through legitimate institutional channels. Any attempt at reform is framed as sacrilege against Lycurgus's divine order and meets suppression as treason. Their only lever is extra-constitutional action (revolution) or rhetorical reframing (claiming they are 'returning' to true Lycurgan intent), both costly and often fatal.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, constitutional_reformers, excluded,
    moderate, biographical, identity_locked, local).

% Legitimizes the unchangeability claim via divine origin narrative (Lycurgus received the laws from Apollo at Delphi). They benefit from the immutability doctrine because it centralizes interpretive authority in their hands—only priests can read the divine will—and prevents lay revision. They collude with the ephorate to suppress reform rhetoric as impiety. They have substantial practical influence through oracular pronouncements but cannot formally revise without undermining their own epistemic authority.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, priesthood_and_oracles, beneficiary,
    institutional, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, priesthood_and_oracles, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, old_spartiate_families).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lycurgan laws coordinate military preparedness and cohesion: communal living, uniform education, prohibition of luxury, and strict marriage rules ensure an efficient warrior class. They also coordinate land holding and property stability through unchangeable inheritance rules. The coordination problem solved was: how to maintain a small warrior republic in a hostile environment without the internal class conflict that destroyed other Greek cities.
% TRANSFER_FUNCTION: Transfers the economic surplus produced by helot and perioikoi labor to Spartiate military training and support; transfers political authority from assembly to gerusian council and ephorate; transfers reproductive freedom (strict marriage rules prevent reproduction of economically marginal sons); transfers land ownership opportunities from younger sons to eldest heirs. The laws move labor, authority, and life choices from the powerless and younger to the entrenched elite.
% ABSENT_VOICES: Helots and perioikoi are structurally excluded from any participation in the agora and cannot propose revision. Younger sons are present but lack voting power proportional to their interest in reform. Constitutional reformers exist but are excluded by frame—any proposal to revise is immediately declared impious. External military rivals have no seat in the assembly. Future generations are absent by definition and cannot consent to the rigid system they inherit.
% DISAPPEARANCE_RATIONALE: If Lycurgan immutability disappeared and the laws became revisable, Sparta would restructure immediately: inheritance rules would allow property division; marriage restrictions would be loosened to increase Spartiate population; economic mobility would emerge; labor obligations on helots would face pressure for revision. The system as constituted depends entirely on the belief that the laws cannot be changed—remove that constraint and the whole architecture collapses into contestation.
% FOUNDING_PROBLEM: Early Sparta (7th-6th century BCE) faced near-constant helot revolt and internal class conflict among Spartiates themselves over land and wealth inequality. Lycurgus's reform (legendary or semi-legendary) imposed radical equality among Spartiates, communal living, military focus, and unchangeable laws to prevent backsliding into internal conflict. The founding problem was: prevent Sparta from dissolving into civil war like other Greek cities.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and even contemporaries from adjacent Greek cities acknowledged the founding problem was solved—Sparta achieved extraordinary internal stability and military reputation. However, that stability required unchangeability. By the 4th century BCE, the founding problem of internal conflict was moot, but the solution (immutability) had created a new, worse problem: demographic collapse. External historians (Plutarch, Polybius, Aristotle) and internal reformers like Agis IV all recognized that the laws that solved the original crisis now perpetuated unsustainability. The founding problem is dead; the constraint persists.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) and rising because the system increasingly extracts from younger Spartiates and future generations as population declines—the unchanged laws continue to enforce restrictions that were sustainable at higher population but become grotesque at lower population. Suppression is very high (0.89) because the immutability doctrine is enforced through frame suppression: any proposal to revise is immediately characterized as impiety/sacrilege/betrayal of Lycurgus. The legal channels for reform do not exist by design. Theater ratio rises from 0.35 to 0.62 over the interval because, as the founding problem becomes obviously dead and the population crisis becomes obvious, the ephorate increasingly engages in interpretive theater (reinterpreting existing law, claiming flexibility) while maintaining the immutability frame. Accessibility collapse is very high (0.91) because the immutability doctrine forecloses alternatives: the rules cannot be changed through legitimate institutional channels; the only alternatives are violent revolution or exit. Resistance is moderate (0.47) because the system is too deeply institutionalized and the suppression too effective for sustained resistance to organize—reformers are isolated, younger Spartiates drift into mercenary service or emigration, helots lack coordination capacity for sustained revolt.
 *
 * PERSPECTIVAL GAP:
 *   The ephorate and old families experience the laws as protecting a hard-won order that maintains stability and their own position. Younger Spartiates and future generations experience them as a trap that tightens with each generation. The reformers (like Agis IV) understand both sides but cannot translate understanding into institutional action because the immutability frame forecloses it. The constraint computes differently by seat: the agenda-setter sees a stable system whose unchangeability is feature (sacred), not bug; the powerless see a death spiral whose mechanism is precisely that unchangeability. The engine's per-seat computation captures this asymmetry from the directionality structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Old Spartiate families are near full beneficiary (d ~ 0.15): they collect from the constraint, face no exit pressure, and have mobile exit options that they do not need. Younger Spartiates are near full target (d ~ 0.85): they are trapped by unchangeable inheritance law, cannot revise it through legitimate channels, and face constrained exit (must leave Sparta entirely to escape). Non-Spartiates are full targets (d ~ 0.95): powerless, trapped, unable to participate in any revision. Future generations are targets by structural lock-in (d ~ 0.90): they inherit the system with no institutional path to adapt it. The ephorate sits at d ~ 0.35 (moderate payer): they administer the system and derive authority from it, but if population collapse reaches critical mass, the system's legitimacy collapses and their authority with it. They are partly benefiting (control, authority) and partly paying (system fragility).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent internal conflict and helot revolt) was solved completely by Lycurgan laws. That solution required unchangeability—any revision would have reintroduced the pressure toward factionalism. But unchangeability has now created a new, worse problem: demographic collapse driven by laws that cannot adapt to population decline. The mandate (maintain internal stability and military power) is increasingly incompatible with the mechanism (unchangeable law). Historical reformers like Agis IV and Cleomenes III tried to solve the crisis by reinterpreting the laws (claiming they were 'returning' to true Lycurgan intent, which allows flexibility) or by constitutional innovation (proposing to expand citizenship, revise property law). These maneuvers prove the mandate is mandatrophic: the institution built to solve one crisis has become the mechanism that prevents solving the new crisis. The constraint persists long after the founding problem is dead, protected by the immutability frame itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_mechanism_empirical,
    'Is the immutability of Lycurgan law a genuine structural constraint (no institutional path to revision), or does covert interpretation and informal revision occur in practice?',
    'Close analysis of historical practice: do the laws in practice remain unchanged, or do successive interpreters (ephorate, priesthood) effectively modify them while claiming immutability? Did marriage rules, property law, or military organization actually shift across centuries, masked by immutability rhetoric?',
    'If immutability is genuine (no revision occurs), the demographic trap is structurally real and unavoidable—Sparta is locked into decline. If covert revision occurs, the system has hidden adaptive capacity and the trap is less absolute; immutability becomes theater masking adaptation (adaptive_fiction reading becomes more plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_mechanism_empirical, empirical, 'Whether Lycurgan immutability is real structural constraint or masked flexibility.').

omega_variable(
    founding_problem_substitution,
    'Did the founding problem (internal conflict and helot revolt) remain live throughout Sparta''s history, or did it genuinely die and Sparta simply continued the system from institutional inertia?',
    'Historical narrative: track internal factionalism and helot revolts across centuries. If both decline sharply after the 5th century and never recur, the founding problem is dead. If persistent factionalism emerges (as it did among Spartiates competing for reform), the founding problem mutated but did not die.',
    'If dead, the constraint becomes a pure piton/theater—system persisting despite its founding justification evaporating. If the problem mutated (factionalism returns as elite reformers vs. conservatives), the system solves a modified version of the founding problem and some extraction is coordination cost, not pure rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_substitution, empirical, 'Whether the founding problem truly died or merely transformed.').

omega_variable(
    suppression_internalization,
    'Is the suppression of reform proposals primarily structural (no institutional path to revision exists) or internalized (Spartiates believe revision is impious, making them self-suppress)?',
    'Comparative frame analysis: when Spartiates travel or interact with other Greeks, do they express desire to revise Lycurgan law but fear acting on it (internalized), or do they genuinely not conceive of revision as possible (structural). Second resolution: examine reform attempts (Agis IV, Cleomenes III)—do they frame their proposals as restoration (revealing internalized constraint) or as unprecedented innovation (revealing structural constraint).',
    'If suppression is primarily internalized, Spartiates carry the constraint with them even if external pressure removes; the system is more resilient to external change. If structural, revision by external conquest or institutional innovation from without becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of reform is structural or internalized.').

omega_variable(
    alternative_readings_foreclosure,
    'Do the demographic-trap reading and the sacral_fidelity reading logically foreclose each other, or do they coexist as competing interpretations of the same facts?',
    'Examine whether accepting immutability as divine mandate (sacral reading) logically requires rejecting the empirical claim that immutability causes demographic collapse (demographic trap reading). If the readings disagree only on *value* (divine mandate is good vs. bad) but agree on *facts* (immutability is real and causes collapse), they coexist. If they disagree on *facts* (one says immutability is real, one says it is theater), they foreclose each other.',
    'If foreclosure is real, only one reading can be true within any single framework. If coexistence holds, both readings can be valid simultaneously (one party accepts the collapse as divine will, another rejects it as unacceptable cost). The engine''s reading_relations inference will depend on resolving this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Logical relationship between demographic-trap and sacral-fidelity readings.').

omega_variable(
    population_decline_causation,
    'Is the population decline of Spartiates primarily caused by unchangeable Lycurgan laws (inheritance rules, marriage restrictions, military-service requirements) or by external factors (wars, emigration, loss of military supremacy making military service less attractive)?',
    'Demographic analysis: compare Spartiate population trends against external shocks (wars, defeats) and internal policy changes. If population declines correlate with wars more than policy, external factors dominate. If population decline is sustained despite military success, internal policy dominates. Natural experiment: did any reforms (even informal ones) correlate with population recovery?',
    'If external factors dominate, the immutability of law is not the primary cause of collapse—other constraints matter more. If internal policy dominates, immutability is the critical bottleneck. The classification of the law as snare (pure extraction with no coordination function) depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(population_decline_causation, empirical, 'Whether demographic collapse is caused by Lycurgan law or external factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lycu_tr_t0, projected).
narrative_ontology:measurement(lycu_tr_t5, lycurgan_laws__demographic_trap_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(lycu_tr_t5, observed).
narrative_ontology:measurement(lycu_tr_t10, lycurgan_laws__demographic_trap_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(lycu_tr_t10, observed).
narrative_ontology:measurement(lycu_tr_t15, lycurgan_laws__demographic_trap_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(lycu_tr_t15, observed).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__demographic_trap_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement_basis(lycu_tr_t20, observed).
narrative_ontology:measurement(lycu_tr_t25, lycurgan_laws__demographic_trap_reading, theater_ratio, 25, 0.59).
narrative_ontology:measurement_basis(lycu_tr_t25, observed).
narrative_ontology:measurement(lycu_tr_t30, lycurgan_laws__demographic_trap_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(lycu_tr_t30, observed).
narrative_ontology:measurement(lycu_tr_t35, lycurgan_laws__demographic_trap_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement_basis(lycu_tr_t35, observed).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__demographic_trap_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(lycu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(lycu_be_t0, projected).
narrative_ontology:measurement(lycu_be_t5, lycurgan_laws__demographic_trap_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(lycu_be_t5, observed).
narrative_ontology:measurement(lycu_be_t10, lycurgan_laws__demographic_trap_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(lycu_be_t10, observed).
narrative_ontology:measurement(lycu_be_t15, lycurgan_laws__demographic_trap_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(lycu_be_t15, observed).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__demographic_trap_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(lycu_be_t20, observed).
narrative_ontology:measurement(lycu_be_t25, lycurgan_laws__demographic_trap_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(lycu_be_t25, observed).
narrative_ontology:measurement(lycu_be_t30, lycurgan_laws__demographic_trap_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(lycu_be_t30, observed).
narrative_ontology:measurement(lycu_be_t35, lycurgan_laws__demographic_trap_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(lycu_be_t35, observed).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__demographic_trap_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(lycu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(lycu_su_t0, projected).
narrative_ontology:measurement(lycu_su_t5, lycurgan_laws__demographic_trap_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement_basis(lycu_su_t5, observed).
narrative_ontology:measurement(lycu_su_t10, lycurgan_laws__demographic_trap_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement_basis(lycu_su_t10, observed).
narrative_ontology:measurement(lycu_su_t15, lycurgan_laws__demographic_trap_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(lycu_su_t15, observed).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__demographic_trap_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(lycu_su_t20, observed).
narrative_ontology:measurement(lycu_su_t25, lycurgan_laws__demographic_trap_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(lycu_su_t25, observed).
narrative_ontology:measurement(lycu_su_t30, lycurgan_laws__demographic_trap_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement_basis(lycu_su_t30, observed).
narrative_ontology:measurement(lycu_su_t35, lycurgan_laws__demographic_trap_reading, suppression_requirement, 35, 0.89).
narrative_ontology:measurement_basis(lycu_su_t35, observed).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__demographic_trap_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement_basis(lycu_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.25).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% Three readings of the Lycurgan kernel: sacral_fidelity_reading treats immutability as divine mandate (mountain-like legitimacy); adaptive_fiction_reading treats immutability claim as mask over covert adaptation (piton-like theater); demographic_trap_reading treats immutability as structural death trap (snare). The readings disagree on whether immutability is real, sacred, or theatrical—not on what the laws are. Each story is independent with its own ε-invariant referent: the standing arrangement assessed by each reading's own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
