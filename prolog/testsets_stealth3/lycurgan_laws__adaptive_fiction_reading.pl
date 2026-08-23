% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability Doctrine as Noble Lie Masking Covert Adaptation (Adaptive-Fiction Reading)
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   Sparta's constitutional order professed itself the unchangeable gift of
 *   the founder Lycurgus: oracle-sanctioned at Delphi, deliberately left
 *   unwritten, and sealed by a perpetual oath said to have been engineered
 *   when Lycurgus bound the city to keep his laws until he returned, then
 *   starved himself to death so the oath could never lapse. This story
 *   instantiates the ADAPTIVE-FICTION READING of that kernel: the profession
 *   of immutability operated as a coordinating noble lie beneath which the
 *   ephorate and the gerousia continuously adjusted practice through
 *   interpretation -- retroactively attributing innovations to 'the ancestral
 *   laws,' suspending rules in emergencies, prosecuting kings who overstepped
 *   -- while the costs of each adjustment landed on those with no seat in the
 *   interpretive circle. On this reading the constitutional rhetoric carried
 *   a mountain-grade claim (fixed, sacred, beyond revision) over a rope-grade
 *   operational reality (working institutional flexibility), and the
 *   celebrated demographic decline is coded as enforcement failure -- the
 *   erosion of the machinery that made the fiction credible -- rather than as
 *   the price of genuine rigidity. The claim and the metrics are independent
 *   authored facts: claimed_type is authored from the structure this reading
 *   asserts (real coordination function plus real asymmetric extraction,
 *   actively enforced); the metrics describe the arrangement's actual
 *   operation as this reading assesses it. This file is one member of a
 *   three-reading constraint family over the lycurgan_laws kernel; the
 *   siblings are linked via network.affects_constraints and the decomposition
 *   is documented in network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - spartiate_citizen_assembly: Dual-positioned participant (organized/identity_locked) -- collects stability and peer privilege; bears agoge discipline, mess dues, and a formally sovereign but practically nullified voice
 *   - ephorate_college: Primary agenda-setter and receipt seat (institutional/constrained) -- operates interpretation of the unwritten corpus, collects the change-control rents, prosecutes even kings
 *   - gerousia_elders: Secondary agenda-setter (institutional/identity_locked) -- life-tenured guardians of the unwritten tradition, adjourn the assembly when it votes crookedly
 *   - heraclid_dual_kingship: Sanctified beneficiary bound by the same fiction (powerful/identity_locked) -- throne consecrated by the doctrine, autonomy capped by ephoral oversight and monthly oaths
 *   - helot_population: Primary target (powerless/trapped) -- bears the full material weight of the sealed order with no address inside it
 *   - impoverished_spartiates: Target whose exclusion is laundered as fidelity (powerless/identity_locked) -- stripped of citizenship as allotments concentrated while redistribution stayed unspeakable
 *   - perioikoi_communities: Excluded contributor (moderate/constrained) -- fights, pays, and trades for the order without a vote or petition path
 *   - peloponnesian_league_allies: Excluded treaty partner (organized/constrained) -- experiences Spartan commitments as movable targets wrapped in ancestral-law language
 *   - classical_analysts: Analytical observer (analytical/analytical) -- Thucydides, Xenophon, Aristotle, Plutarch and their modern successors reconstruct the gap between profession and practice from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.6).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability Doctrine as Noble Lie Masking Covert Adaptation (Adaptive-Fiction Reading)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb').
narrative_ontology:cs_kernel_codification('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', fixed_text).
narrative_ontology:cs_authority_grounding('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', lineage).
narrative_ontology:cs_interpretation_layer_present('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb').
narrative_ontology:cs_reading_relation('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', foundational, immutability_rhetoric_is_instrumental_fiction).
narrative_ontology:cs_axiom_status(immutability_rhetoric_is_instrumental_fiction, holdable).
narrative_ontology:cs_axiom_grounding('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', immutability_rhetoric_is_instrumental_fiction, empirically_contingent).
narrative_ontology:cs_axiom('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', secondary, interpretation_is_the_operative_channel_of_change).
narrative_ontology:cs_axiom_status(interpretation_is_the_operative_channel_of_change, holdable).
narrative_ontology:cs_axiom_grounding('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', interpretation_is_the_operative_channel_of_change, conventional).
narrative_ontology:cs_reference_frame('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', professed_charter_over_adaptive_practice).
narrative_ontology:cs_drift_state('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', fourth_century_retrospect, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab0ea96e-91a9-47b7-9fd4-fbd64c771fdb', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_assembly).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate_college).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, heraclid_dual_kingship).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, impoverished_spartiates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_assembly).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, heraclid_dual_kingship).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, lycurgan_founder_authority).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, delphic_oracle_sanction).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, unwritten_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult male Spartiates who ratified measures in open acclamation, drew standing from the equal-portions ideal, ate at common messes funded by helot-worked allotments, and trained continuously for war. They hold a formal right to accept or reject proposals, but the council of elders can dismiss them without a vote when they answer crookedly. Leaving means forfeiting mess membership and therefore citizenship -- social death inside the only community their formation fits them for.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_assembly, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_assembly, payer).

% Five annually elected magistrates who preside over the assembly, supervise the kings, may imprison and prosecute them, receive foreign envoys, and decide in disputed cases what counts as the ancestral law. Their rulings adjust practice while invoking the founder's name; when their year ends they step back into ordinary citizen life under the same rules they administered.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate_college, agenda_setter,
    institutional, immediate, constrained, regional).

% Twenty-eight elders over sixty plus the two kings, chosen for life, who prepare all business brought before the assembly and judge capital cases. They are the living archive of an intentionally unwritten tradition -- their authority depends on there being no rival written text -- and they are selected precisely for embodying continuity, so stepping outside the tradition is unthinkable for them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia_elders, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, gerousia_elders, beneficiary).

% Two hereditary kings of the Agiad and Eurypontid houses, chief priests and army commanders, who swear each month to uphold the laws while the ephors swear on the city's behalf. The doctrine consecrates their throne; the same doctrine chains them -- ephors watch them, repeated campaign failure costs them office, and one regent was prosecuted to death. Dynastic exit does not exist.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, heraclid_dual_kingship, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, heraclid_dual_kingship, payer).

% State serfs of Laconia and Messenia bound to the plots that feed the citizen messes, delivering fixed quotas, enduring the secret squads that cull them annually, and reachable toward freedom only through war service. The constitution they feed explicitly names them outside the political community forever, and flight means outlawry or death.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, biographical, trapped, regional).

% Descendants of peers who can no longer pay their mess contributions after allotments concentrate in fewer hands; they are struck from the citizen rolls while remaining culturally Spartiate to the bone. Because the laws are held perfect and beyond revision, the redistribution that originally created the peerage can never be re-enacted openly to save them -- their exclusion is presented as fidelity to the founder rather than admitted as policy failure.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, impoverished_spartiates, payer,
    powerless, biographical, identity_locked, regional).

% Free inhabitants of the Laconian and Messenian towns -- traders, craftsmen, marines -- who fight in Spartan armies and pay dues but hold no vote and no forum. When policy shifts under the magistrates' reinterpretations, they absorb the consequences in campaigns, tribute, and disrupted commerce with no channel to contest any of it.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities, excluded,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities, payer).

% Allied cities bound by bilateral treaties to follow Sparta to war. They experience Spartan commitments as movable targets: obligations are honored or reinterpreted as the ancestral laws supposedly require, and there is no shared court in which to press an appeal. Several allies later cite exactly this arbitrariness when they defect.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, peloponnesian_league_allies, excluded,
    organized, biographical, constrained, regional).

% Thucydides, Xenophon, Aristotle, Plutarch, and their modern successors reconstruct the system from outside: setting the official account of unchanging laws beside the documented record of reinterpretation, royal prosecutions, and shrinking citizen rolls, they can name the gap as a gap without bearing any of its costs.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, classical_analysts, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, ephorate_college).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a small armed citizen minority a single unchallengeable normative reference point: it stabilizes expectations across generations, deters factional campaigns to rewrite the constitution (no one can stand for office promising to change what cannot be changed), and lets necessary adjustments proceed as authorized interpretation without triggering the legitimacy crisis that open amendment would provoke.
% TRANSFER_FUNCTION: Moves constitutional change-control from the citizen body at large to a rotating magistracy and a life-tenured council; moves the agricultural surplus of the helot districts to the citizen messes under terms framed as eternal rather than negotiable; and moves the costs of each adaptation -- lost citizenship, campaign burdens, fixed quotas -- onto those with no seat in the interpretive circle.
% ABSENT_VOICES: Helots have no assembly, no petition, and no recognized speaker; their objection survives only in revolt records and hostile observers. Impoverished ex-citizens retain kin inside the system but no formal voice. Perioikoi and allies negotiate only bilaterally with magistrates who answer to no shared court. Unanimity inside the walls was purchased partly by these absences.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the citizen body would face immediate constitutional crisis: with no sacred reference point, land redistribution becomes instantly proposable, the kings lose their sacral cover, the ephors lose their interpretive monopoly, and the ideological seal on the helot terms breaks -- the entire garrison settlement would have to be renegotiated from scratch.
% FOUNDING_PROBLEM: How a tiny citizen body -- never more than a few thousand peers amid a servile population many times larger -- could hold its conquests, maintain working equality among its warriors, and survive generational turnover without factional dissolution: a combined security-and-cohesion problem for a garrison society.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Aristotle's Politics treats the system's defects (shrinking citizen numbers, endemic hostility of the serf population) as structural rather than accidental; Thucydides documents the opacity and adaptivity of Spartan decision-making; and the recurring helot insurrections attest the liveness of the founding problem from below. No attestation rests on the beneficiaries alone.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: substantial extraction rides on genuine coordination. The fiction's specific yields were asymmetric -- blocked redistributive correction as land concentrated (creating the hypomeiones), laundered exclusion of failing citizens as fidelity to Lycurgus, and concentrated change-control in the interpretive magistracies -- while the stability goods were broadly shared among citizens. It is not higher because the doctrine's marginal load sits on insiders' voice and correction, not on the helot economy itself, which is the broader order's extraction rather than the doctrine's. Suppression 0.62 is the interval-end state: the scalar is a raw structural property (unscaled by power or scope -- scaling happens only to extractiveness in the engine); the temporal series shows the honest arc, a ratchet to 0.74 as each accumulated contradiction required more enforcement to paper over, then decay to 0.62 as the machinery eroded. Theater_ratio 0.50: the monthly king-ephor oath exchanges, the retroactive 'Lycurgan' attributions of novelties, and the ceremonial invocations of the founder grew increasingly performative as the profession-practice gap widened -- the series ends exactly at the Goodhart drift threshold, which is a symptom to note, not a classification test. Accessibility_collapse 0.62: inside the system alternatives collapse far (no written text to appeal to, amendment unspeakable, expulsion for mess arrears), but the fiction is legible to outsiders and corrodes once seen, so it is well short of natural-law completeness. Resistance 0.45: open internal challenge was rare -- that rarity is the fiction's success -- but the order it sealed met chronic helot insurgency and, late, reformist shocks that had to appeal TO Lycurgus in order to change anything (the strongest possible testimony to the fiction's grip). Boltzmann: coordination_type identity_coordination, because the doctrine's dominant function is boundary maintenance -- defining who counts as a peer (syssitia membership, agoge completion), policing membership against evolving criteria, and anchoring the reputation economy of reverence for the laws. The FNL gaming risk is acute here and acknowledged: this constraint literally weaponizes identity framing ('this is who we are'), so the type's complexity offset must not excuse its coupling profile -- the extraction that matters concentrates on identity-locked insiders (hypomeiones) and addressless outsiders (helots), which is exactly the Power-x-Scope shape the guidance flags for scrutiny. All three temporal series run on one shared seven-point grid (t=0 approximates the traditional founding era after the First Messenian consolidation; t=450 approximates 371 BC and Leuctra; steps of roughly 75 years, two to three generations), so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is the measurement. From the ephorate seat the arrangement is the machine of legitimate governance those magistrates personally operated -- annual office, real discretion, prosecutorial power reaching kings; a coordination instrument they ran. From the helot seat the same arrangement is a sealed order with no address: the terms under which they fed the messes were framed as eternal precisely so they could never be renegotiated. From the hypomeion seat it is a promise retroactively revoked -- equal portions proclaimed immutable, then quietly unavailable. From the kings' seat it is a throne that sanctifies and shackles at once. From the allied seat it is an unpredictable counterparty hiding behind ancestral-law language. The engine computes these per-seat classifications from the structural data; this story's authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The interpretive elite (ephorate_college, gerousia_elders) derives low directionality from its beneficiary declarations -- the doctrine subsidizes them with change-control rents and sacral cover. The helot_population and impoverished_spartiates derive high directionality from their victim declarations, hardened by trapped and identity_locked exits. Two overrides are declared because the derivation from beneficiary/victim data alone would misplace dual-positioned seats. First, spartiate_citizen_assembly (power atom: organized): the derivation would read the beneficiary declaration as d near 0.15-0.2, but the citizen body also bears agoge discipline, lifelong mess dues, constant campaign exposure, and a formally sovereign voice nullified by gerousia adjournment -- its true position is near symmetric, overridden to 0.5. Second, heraclid_dual_kingship (power atom: powerful): the derivation would read the beneficiary declaration as strongly subsidized, but the kings are the fiction's consecrated priests AND its oath-bound subjects -- monthly oaths, ephoral surveillance, prosecution unto death (Pausanias) -- overridden to 0.45. These are asymmetric institutional relationships the structural derivation cannot distinguish from the arrays alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against the pure-snare reading: the fiction solved a real collective-action problem -- how a small armed minority holds conquests and internal equality across generations without factional constitutional entrepreneurship -- and delivered net benefits to the citizen body for centuries; a pure-extraction coding misses the coordination core that made the arrangement durable. Against the pure-rope reading: the same structure concentrated change-control in a narrow magistracy, laundered the exclusion of impoverished citizens as piety, and sealed the helot terms beyond negotiation; a coordination-only coding misses the asymmetry that required active enforcement (oath architecture, sacralization, surveillance, prosecution) to hold. On mandatrophy proper: the founding problem stayed live for the whole interval -- it was never solved and abandoned, it failed catastrophically -- so no resolved-mandatrophy declaration is authored, and the mismatch consumer should find status=live paired with world_rearranges, correctly producing no zombie flag: this arrangement died WITH its problem unresolved, which is failure, not atrophy. The rising theater series marks pre-atrophic strain (performance substituting for function as the gap widened), not completed atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the adaptive_fiction_reading of the lycurgan_laws kernel; the sibling readings sacral_fidelity_reading and demographic_trap_reading instantiate different constraints over the same corpus. Which reading''s structural premises does the surviving evidence actually support?',
    'Comparative analysis of attribution practice (retroactive ''Lycurgan'' citations for documented innovations), the literary and epigraphic record of ephoral reinterpretation and royal prosecution, and the testimony of Thucydides, Xenophon, and Aristotle on whether contemporaries treated the laws as literally binding.',
    'If the sincere-divine-norm reading wins, epsilon falls toward rope levels and the victim set shrinks to punished dissenters; if the literal-binding reading wins, epsilon rises and the demographic_trap causal coding gains ground. This file''s tangled_rope classification stands only under the fiction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure routed here per the kernel-reading rules: which reading of the Lycurgan kernel the evidence supports, and what each sibling would change structurally.').

omega_variable(
    enforcement_failure_vs_rigidity,
    'Did Spartan demographic decline (oliganthropia) result from failure of the enforcement machinery that made the immutability fiction credible, as this reading asserts, or from genuine unrevisability blocking corrective reform, as the demographic_trap sibling asserts?',
    'Population and landholding reconstructions correlated with dated syssitia expulsions, allotment concentration, and documented ephoral interventions; the test is whether adaptation capacity existed and failed, or never existed.',
    'Confirms or overturns this reading''s causal delta. If rigidity causality wins, the demographic_trap sibling absorbs this story''s explanatory weight and this constraint''s coordination credit shrinks accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_failure_vs_rigidity, empirical, 'Cause-of-collapse contest between this reading and its demographic_trap sibling.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was Spartiate fidelity to the immutable laws structurally coerced (ephorate surveillance, prosecution risk, expulsion for mess arrears) or internalized (agoge-formed identity making deviation unthinkable)?',
    'Post-decline observance trajectory: Xenophon''s Constitution of the Lacedaemonians ch.14 records eager obedience lapsing as enforcement relaxed. If observance tracked enforcement capacity, suppression was predominantly structural; if it persisted independently of enforcement, it was substantially internalized.',
    'If internalized, the scalar suppression understates the fiction''s historical grip and the identity_locked exit ratings harden; if structural, the fiction always depended on machinery, which supports this reading''s enforcement-failure causal chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the measured suppression: structural coercion versus internalized reverence.').

omega_variable(
    designed_fiction_vs_accreted_convention,
    'Was the perpetual-oath architecture -- the oath-trap by which Lycurgus allegedly bound the city until his return and then self-starved -- a deliberately engineered noble lie, or a convention that accreted sacral force over generations?',
    'Source-critical analysis of the Plutarchan tradition against earlier attestations (Tyrtaios fragments, the Herodotean account of the Great Rhetra), looking for design signatures such as self-destruct clause engineering versus gradual ritualization.',
    'Deliberate design strengthens the instrumental-fiction axiom and raises the intent-level assessment of the arrangement; accretion supports a more benign reading in which no architect intended the asymmetry that emerged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(designed_fiction_vs_accreted_convention, conceptual, 'Intentionality of the fiction''s founding architecture: engineered trap versus accreted sanctity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t0, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t75, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 75, 0.16).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t75, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t150, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t150, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t225, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 225, 0.29).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t225, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t300, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 300, 0.37).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t300, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t375, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 375, 0.45).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t375, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_tr_t450, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 450, 0.5).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_tr_t450, observed).

% Extraction over time
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t0, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t75, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 75, 0.44).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t75, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t150, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 150, 0.5).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t150, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t225, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 225, 0.56).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t225, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t300, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 300, 0.63).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t300, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t375, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 375, 0.66).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t375, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_be_t450, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 450, 0.6).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_be_t450, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t0, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t75, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t75, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t150, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 150, 0.65).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t150, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t225, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 225, 0.7).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t225, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t300, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 300, 0.74).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t300, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t375, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 375, 0.71).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t375, observed).
narrative_ontology:measurement(lycurgan_adaptive_fiction_su_t450, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 450, 0.62).
narrative_ontology:measurement_basis(lycurgan_adaptive_fiction_su_t450, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Lycurgan constitution' conflates three structurally distinct claims, which are authored as three linked stories over the lycurgan_laws kernel. This file authors the adaptive_fiction_reading (immutability rhetoric as instrumental fiction over working interpretive flexibility; epsilon 0.60, tangled_rope). The sacral_fidelity_reading authors the professed frame itself as sincerely divine ordinance (different epsilon, different victim set -- punished dissenters rather than laundered exclusions). The demographic_trap_reading authors the same collapse record with opposite causal coding (unrevisability as fatal). The sacral_fidelity member is upstream: the professed frame it describes is the very rhetoric this reading analyzes as instrument, and pious citation of that frame was the mechanism by which adaptation was concealed. The demographic_trap member is downstream of both, consuming the same fourth-century record. Each member carries its own stable epsilon; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, organized, 0.5).
constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
