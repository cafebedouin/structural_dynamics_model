% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Embedded Ritual Transmission of Catastrophe-Survival Competence (Form/Function Fused Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the HYBRID EMBEDDED reading of the
 *   catastrophe_memory_transmission kernel: survival competence and symbolic
 *   form are treated as co-constitutive, not as two separable things
 *   (function encoded IN form vs. form preserving identity ALONGSIDE
 *   function). In this reading the ritual's operational capacity does not
 *   exist independently of its enacted symbolic sequence — you cannot extract
 *   'the competence' and discard 'the form' without degrading the competence
 *   itself, because the transmission channel is non-propositional (embodied
 *   practice, not instruction). This produces a rope-with-mountain-substrate
 *   structural profile: genuine coordination function (rope) resting on an
 *   embodied-cognition constraint that is not a policy choice but closer to a
 *   physical/cognitive fact about how non-propositional skill transmits
 *   across generations without literacy infrastructure (mountain-like
 *   substrate). There is no identified victim class under this reading
 *   because no party is structurally extracted from — the cost borne (time,
 *   discipline, difficulty of change) is the cost of the coordination
 *   function itself, not a transfer to a beneficiary. This is a SIBLING of,
 *   not identical to, operational_competence_reading (which treats the ritual
 *   as decomposable into extractable operational content — closer to
 *   information transfer that COULD in principle be re-encoded in a manual)
 *   and symbol_continuity_reading (which treats the symbolic/identity
 *   function as the primary survival mechanism, with operational competence
 *   as secondary or emergent). All three are separate constraints sharing one
 *   kernel; do not average across them.
 *
 * KEY AGENTS:
 *   - ritual_elders: agenda_setter/beneficiary (moderate/identity_locked) — carry the embodied sequence; cannot fully externalize it
 *   - practicing_community_members: beneficiary (moderate/constrained) — rehearse the coordination function through participation
 *   - children_and_novices: beneficiary (powerless/constrained) — receive competence and form together, inseparably, before propositional understanding
 *   - outside_ethnographers_and_disaster_researchers: observer (analytical/analytical) — corroborate the coordination function's real-world efficacy from outside
 *   - would_be_reformers: excluded (powerless/constrained) — propose separating form from function to increase legibility; unheard within this reading's own logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Embedded Ritual Transmission of Catastrophe-Survival Competence (Form/Function Fused Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '4f255658-3abd-40de-8f79-495bd96f1352').
narrative_ontology:cs_kernel_codification('4f255658-3abd-40de-8f79-495bd96f1352', implicit).
narrative_ontology:cs_authority_grounding('4f255658-3abd-40de-8f79-495bd96f1352', practice).
narrative_ontology:cs_interpretation_layer_present('4f255658-3abd-40de-8f79-495bd96f1352').
narrative_ontology:cs_reading_relation('4f255658-3abd-40de-8f79-495bd96f1352', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f255658-3abd-40de-8f79-495bd96f1352', catastrophe_memory_transmission__symbol_continuity_reading, influences).
narrative_ontology:cs_axiom('4f255658-3abd-40de-8f79-495bd96f1352', foundational, form_function_coconstitution).
narrative_ontology:cs_axiom_status(form_function_coconstitution, holdable).
narrative_ontology:cs_axiom_grounding('4f255658-3abd-40de-8f79-495bd96f1352', form_function_coconstitution, empirically_contingent).
narrative_ontology:cs_axiom('4f255658-3abd-40de-8f79-495bd96f1352', secondary, non_propositional_transmission_irreducibility).
narrative_ontology:cs_axiom_status(non_propositional_transmission_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('4f255658-3abd-40de-8f79-495bd96f1352', non_propositional_transmission_irreducibility, empirically_contingent).
narrative_ontology:cs_reference_frame('4f255658-3abd-40de-8f79-495bd96f1352', embodied_transmission_as_original_channel).
narrative_ontology:cs_drift_state('4f255658-3abd-40de-8f79-495bd96f1352', contemporary_literacy_and_technology_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4f255658-3abd-40de-8f79-495bd96f1352', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations_facing_recurrence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, children_and_novices).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_cognition_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, non_propositional_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and enact the precise sequence of gestures, chants, spatial arrangements, and timing that constitute the ritual. They do not merely narrate what to do in a catastrophe (flood, famine, storm surge, seismic event) — their bodies carry the choreography that IS the coordination plan: who moves where, what is checked, in what order, under what conditions. They cannot fully articulate the rule set in propositional language; asked to explain, they perform. Their exit from the role means the specific embodied sequence is lost, not merely retold badly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_elders, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_elders, beneficiary).

% Participate in the ritual cycle across the year, rehearsing roles that map directly onto division of labor, evacuation routes, resource caching, and threat-signal recognition during an actual catastrophe. Their participation is costly in time and discipline but the coordination capacity it produces has no cheaper substitute they have found; the ritual's repetition is what keeps the operational sequence load-bearing in muscle memory rather than fading into inert story.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Are inducted into the ritual through years of repeated, only partly explained participation before they are told 'why.' They receive the operational competence before they receive (or in place of) a propositional account of it. If they left the community before full induction, they would carry fragments of the form without the embedded competence — the two do not transmit separately.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, children_and_novices, beneficiary,
    powerless, civilizational, constrained, local).

% Study the ritual from outside, documenting correlations between ritual fidelity and survival outcomes across recorded catastrophic events. They can describe the coordination function in propositional terms but note their descriptions are lossy relative to what practitioners actually carry — the map is not the embedded competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, outside_ethnographers_and_disaster_researchers, observer,
    analytical, generational, analytical, global).

% Occasionally propose shortening, modernizing, or rationalizing the ritual (e.g., replacing lengthy chant sequences with a printed evacuation manual). They are rarely heard in ritual-governance conversations because the elders and the community treat fidelity to form as inseparable from the competence itself, so proposals to change form register as proposals to discard the competence — the reformers' concern (accessibility, time cost, literacy barriers) is not evaluated on its own terms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, would_be_reformers, excluded,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual cycle rehearses, at fixed intervals, the exact sequence of roles, movements, resource checks, and signal-recognition behaviors that constitute effective collective response to a recurring catastrophe type (flood, storm, famine, seismic event). This is a genuine and non-trivial coordination problem: getting an entire community to execute a complex, time-critical, multi-role response correctly under stress, without a manual, requires the response to be rehearsed until it is embodied rather than merely known.
% TRANSFER_FUNCTION: The ritual transfers embodied operational capacity from elders and experienced practitioners to novices and the next generation, across time, through repeated enacted practice rather than through explicit instruction. Nothing is extracted from one party to benefit another in a zero-sum sense; the transfer is temporal (older to younger) and non-rivalrous — everyone who participates gains the same competence.
% ABSENT_VOICES: Would-be reformers who want a more legible, faster, more literacy-friendly transmission mechanism (e.g., a written manual) are structurally unheard, because within this reading form and function are treated as inseparable — a proposal to change the form registers as a proposal to lose the competence, not as a proposal about efficiency. Their objection is real but is never evaluated on its own terms inside the community's own framework.
% DISAPPEARANCE_RATIONALE: If the ritual cycle stopped tomorrow, the embodied coordination competence would not persist independently of it in this reading — the operational capacity is not stored anywhere else (no manual carries it, because the manual was never the medium). Within a generation the community's actual capacity to execute a coordinated, timely response to the recurring catastrophe would measurably degrade, even if people 'remembered' that the ritual used to exist.
% FOUNDING_PROBLEM: A recurring, high-stakes environmental catastrophe (flood, famine, storm, or seismic event) that requires precise, time-critical, multi-role collective response — and no reliable literate or institutional channel existed to transmit that response across generations, so the community encoded the response inside a repeated symbolic practice that could be transmitted by imitation and repetition.
% FOUNDING_PROBLEM_CORROBORATION: Outside disaster researchers and ethnographers, working from independent event records, corroborate that communities with higher ritual fidelity show measurably better coordinated response during recurrences of the catastrophe type — this is attestation from outside the benefiting community itself. Some community members and reformers dispute whether the SAME operational outcomes could now be achieved more efficiently through modern coordination tools (early warning systems, printed protocols), which would mean the founding problem is only partly still live in its original form.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18, drifting only slightly upward) because no party captures value at another's expense in this reading — the ritual's cost (time, cognitive/physical discipline) is coordination cost, not rent. Suppression is moderate-low (0.22): the main suppressive force is not coercive but epistemic — the reading itself. Alternatives (writing it down, modernizing it) are not fully collapsed but are hard to articulate for the reformers because the community's own framework treats fidelity-to-form as inseparable from competence, so proposals to change form are heard as proposals to abandon function. Accessibility collapse is higher (0.62) than a pure rope typically carries, reflecting the mountain substrate: once you accept the embodied-cognition premise, the practical alternatives (a written manual, a shortened form) genuinely do NOT transmit the same non-propositional competence, so the collapse is partly a real structural fact and partly a framing choice — this ambiguity is the story's central omega. Resistance is low (0.2) — there is little active resistance from within, since most participants experience the ritual as valuable, not extractive; what resistance exists comes from the excluded reformer voice.
 *
 * PERSPECTIVAL GAP:
 *   From the elders' seat (agenda_setter/beneficiary, identity_locked exit) the ritual is simply how competence exists — there is no experienced gap between form and function to interrogate. From the reformers' seat (excluded, constrained exit) the same arrangement looks like an avoidable rigidity: a coordination function trapped inside an unnecessarily costly and inaccessible symbolic shell. From the outside-observer seat (analytical), the arrangement is a genuine and well-corroborated coordination mechanism whose embeddedness is an empirical, not merely rhetorical, claim — disaster-outcome data supports it. The engine should compute these as structurally different experiences of the same authored data, not as competing claims about a shared ε.
 *
 * DIRECTIONALITY LOGIC:
 *   All beneficiaries (elders, community members, children/novices, future generations) sit near the symmetric-to-beneficiary end: they pay real costs (time, discipline, difficult transmission) but receive the corresponding competence, and no other party captures a surplus from their payment. No victims are declared because under this reading there is no structural extraction — the cost is intrinsic to the coordination function, not diverted to an extractive third party. The excluded reformers are not victims of extraction either; they are unheard voices whose concern is about EFFICIENCY of transmission, not about being extracted from — this is why they are modeled as `excluded`, not `payer`.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as CONTESTED rather than flatly 'live,' because outside corroboration (disaster researchers) supports continued efficacy while some internal reformers argue the ORIGINAL founding problem (no literate channel for time-critical coordination) has been partially superseded by modern tools, even if the ritual persists. This prevents two mislabeling errors: (1) treating the ritual as pure inert tradition-for-tradition's-sake (it demonstrably still carries functioning coordination content, per outside corroboration), and (2) treating it as an unimpeachable natural necessity immune to any efficiency critique (the reformer voice and the contested status keep that door open). The disappearance_verdict of world_rearranges anchors the classification against drifting into 'this is just identity performance' (symbol_continuity_reading's territory) — under THIS reading, removing the ritual measurably degrades operational capacity, not merely communal feeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Is the embedded-cognition claim (that operational competence cannot be transmitted without the specific enacted symbolic form) an empirically testable structural fact, or is it a framing commitment internal to the practicing community that outside analysis cannot independently verify?',
    'Controlled comparison: communities that abandon ritual form but attempt to preserve a propositional/written version of the coordination content, tracked against communities maintaining full ritual fidelity, measured against actual catastrophe-response outcomes over multiple recurrence events.',
    'If competence transmits comparably well through non-ritual channels, this reading collapses toward operational_competence_reading (form becomes incidental, accessibility_collapse should be revised sharply downward, and the reformer voice gains legitimate standing). If competence measurably degrades without the specific form, this reading''s mountain-substrate claim is empirically vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Whether form/function inseparability is a testable structural fact or an internal framing commitment.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between the three sibling readings of this kernel actually live — is it a factual dispute about how transmission works, or a values dispute about which function (operational survival vs. communal identity/mourning) matters more when they cannot be cleanly separated?',
    'Compare the three readings'' predictions on a case where operational and identity functions diverge sharply (e.g., a community that survives via modernized coordination but discontinues the mourning-ritual aspect, or vice versa) and see which reading''s predicted outcome actually obtains.',
    'Locates whether the kernel contest is resolvable by evidence (empirical) or is an irreducible framing choice about which good the arrangement primarily serves (conceptual/preference) — this determines whether future evidence could ever fully settle which reading is ''correct'' for a given community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the three-reading kernel dispute is empirical or an irreducible framing/values disagreement.').

omega_variable(
    reformer_exclusion_legitimacy,
    'Is the exclusion of would-be reformers from ritual-governance conversations a legitimate consequence of the embedded-cognition structural fact (their proposal genuinely would degrade the competence), or is it an unexamined conflation that forecloses a hearing they deserve on efficiency/accessibility grounds regardless of the form-function question?',
    'Structured internal deliberation process where reformer proposals are evaluated against the empirical outcome data (see form_function_separability) rather than being pre-judged by the community''s own framing.',
    'If exclusion is unwarranted, this reading understates suppression and the absent_voices dynamic is doing more work than the low suppression score credits it for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_exclusion_legitimacy, preference, 'Whether excluding reformers from governance is structurally warranted or an unexamined foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_transmission kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. hybrid_embedded_reading treats form and function as co-constitutive (rope with mountain substrate, low ε, no victims). operational_competence_reading treats the ritual as substantially decomposable into extractable propositional content (predicting a more rope-like, lower accessibility_collapse profile with less identity_locked exit for elders). symbol_continuity_reading treats communal identity/mourning as the primary good, with operational competence as secondary (predicting different beneficiary framing centered on identity preservation rather than survival competence, and different disappearance_rationale). Do not average ε across the three; each has its own stable value and its own stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
