% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Article II Non-Appropriation: Extraction-Permissive Reading
 *   domain: international_law/space_commons
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) establishes that celestial
 *   bodies are the 'province of all mankind' and cannot be subject to
 *   sovereign territorial claims. The extraction-permissive reading
 *   interprets this ban as applying only to sovereign territorial
 *   appropriation, leaving open the possibility that private corporations can
 *   acquire property rights in extracted resources. This reading has become
 *   state practice among spacefaring powers (USA, Russia, Luxembourg, UAE,
 *   others) without formal treaty amendment or multilateral authorization.
 *   Spacefaring states benefit from attracting extraction investment under
 *   their flags; corporations benefit from uncontested property rights;
 *   non-spacefaring states lose the option value of collective governance and
 *   benefit-sharing. The constraint is CLAIMED as tangled_rope (it
 *   coordinates extraction investment while extracting from excluded parties)
 *   and the authored metrics describe substantially extractive, actively
 *   enforced (via regulatory gatekeeping and suppression of alternative
 *   interpretations) operation.
 *
 * KEY AGENTS:
 *   - Spacefaring states (institutional, agenda-setter): authorize extraction via flag-state licensing; control the reading's enforcement through COPUOS and state practice.
 *   - Resource extraction corporations (powerful, beneficiary): acquire property rights; benefit from uncontested access and absence of benefit-sharing obligations.
 *   - Non-spacefaring states (powerless, payer): structurally excluded from extraction; lose option value of collective governance; identity-locked to UN participation (cannot exit without sovereign cost).
 *   - Conservation advocates (organized, excluded): argue for commons_conservation reading; lack enforcement authority; suppressed by spacefaring-state consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Non-Appropriation: Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'f12c92a6-4693-4294-b16c-e908343baa61').
narrative_ontology:cs_kernel_codification('f12c92a6-4693-4294-b16c-e908343baa61', fixed_text).
narrative_ontology:cs_authority_grounding('f12c92a6-4693-4294-b16c-e908343baa61', extraction).
narrative_ontology:cs_interpretation_layer_present('f12c92a6-4693-4294-b16c-e908343baa61').
narrative_ontology:cs_reading_relation('f12c92a6-4693-4294-b16c-e908343baa61', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('f12c92a6-4693-4294-b16c-e908343baa61', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('f12c92a6-4693-4294-b16c-e908343baa61', foundational, non_appropriation_bars_states_only).
narrative_ontology:cs_axiom_status(non_appropriation_bars_states_only, holdable).
narrative_ontology:cs_axiom_grounding('f12c92a6-4693-4294-b16c-e908343baa61', non_appropriation_bars_states_only, conventional).
narrative_ontology:cs_axiom('f12c92a6-4693-4294-b16c-e908343baa61', secondary, private_extraction_rights_valid_post_removal).
narrative_ontology:cs_axiom_status(private_extraction_rights_valid_post_removal, holdable).
narrative_ontology:cs_axiom_grounding('f12c92a6-4693-4294-b16c-e908343baa61', private_extraction_rights_valid_post_removal, conventional).
narrative_ontology:cs_reference_frame('f12c92a6-4693-4294-b16c-e908343baa61', article_ii_as_state_sovereignty_boundary).
narrative_ontology:cs_drift_state('f12c92a6-4693-4294-b16c-e908343baa61', contemporary_commercial_extraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f12c92a6-4693-4294-b16c-e908343baa61', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, resource_extraction_corporations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_common_interest).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, flag_state_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the technological capability and institutional infrastructure to authorize private resource extraction beyond Earth. Flag-state authority over registered corporations gives them control over who extracts what and from where. They collect no formal royalties but receive diplomatic prestige, industrial base development, and geopolitical leverage from hosting extraction operations. The reading requires no compensation to excluded states, making private ownership sustainable under state sponsorship.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states, agenda_setter,
    institutional, generational, arbitrage, universal).

% Acquire property rights in extracted resources (minerals, water ice, volatiles) without competing bidders, international licensing fees, or compensation obligations. The extraction-permissive reading treats Article II as prohibiting state territorial claims only, leaving private appropriation open. Their exit option is high-grade: they can relocate operations between jurisdictions, negotiate with different flag states, and arbitrage regulatory differences.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, resource_extraction_corporations, beneficiary,
    powerful, biographical, arbitrage, universal).

% Are structurally excluded from participating in resource extraction due to lack of launch capability and high capital requirements. The extraction-permissive reading provides no compensation mechanism, international licensing revenue, or benefit-sharing framework. Their objection to the reading is routed through the United Nations Committee on the Peaceful Uses of Outer Space, where they are outvoted by spacefaring states. Identity-lock operates through UN membership and treaty participation: exit would mean withdrawing from space-governance forums entirely, an unthinkable cost for sovereignty and international standing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, identity_locked, universal).

% A non-agent entity representing the accumulated common interest in preserved celestial environment and equitable access. The extraction-permissive reading privileges current extraction over preservation and common stewardship, shifting costs to unrepresented future actors. This is a narrative device for modeling long-term externality.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_common_interest, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__extraction_permissive, future_generations_common_interest).

% Argue that Article II's non-appropriation principle extends to resource extraction and that any extraction should be gated by international environmental and equity review. They are excluded from treaty amendment processes and enforcement mechanisms, relying on soft-law advocacy and treaty-interpretation campaigns. Their preferred alternative (the commons_conservation reading) is not authorized by current state practice.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, conservation_advocates, excluded,
    organized, civilizational, constrained, universal).

% Regulate corporations registered under their flag and certify compliance with Article II. In the extraction-permissive reading, they authorize extraction by licensing private corporations, collect registration fees and export duties, and benefit from industrial development without bearing the cost of exclusion borne by non-spacefaring states. Their stakes are high but flexible: they can shift corporate registrations and adapt rules to remain competitive as extraction technologies mature.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, flag_state_regulators, agenda_setter,
    institutional, biographical, mobile, universal).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, flag_state_regulators, beneficiary).

% Adjudicates disputes over Article II interpretation when brought by state parties. The extraction-permissive reading is the current working interpretation (as reflected in state practice and licensing regimes); absent a case presenting the alternative reading with sufficient state backing, the court's posture is deference to the spacefaring-state interpretation. Observes but does not initiate reinterpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_court, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear property-rights regime for space resource extraction, removing ambiguity about who can claim extracted materials. Private ownership, once extracted and removed to Earth or orbital depot, is recognized without competing state claims on the same resource body. This clarity enables investment capital to flow to extraction ventures.
% TRANSFER_FUNCTION: Transfers exclusive extraction rights and ownership of extracted resources from the common domain of celestial bodies to spacefaring corporations and their flag states, who capture all value. No mechanism transfers benefit-sharing revenue to non-spacefaring states or common environmental stewardship funds. The transfer is one-way: spacefaring states and their corporations gain; others lose the option value of future collective resource governance.
% ABSENT_VOICES: Non-spacefaring states and conservation advocates are structurally excluded from the reading's authorization process. They cannot launch extraction campaigns or veto extraction by others. Their objections are filed in COPUOS and General Assembly debates but have no enforcement mechanism. Hypothetical future states with newfound launch capability would have to petition spacefaring powers for access rather than claiming rights under a multilateral regime.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive reading evaporated (replaced by a conservation or international-regime reading), extracted resources would cease to be private property and instead would be subject to international benefit-sharing, environmental review, or collective stewardship. Corporations with sunk costs in extraction infrastructure would face write-downs; spacefaring states would lose geopolitical advantage; investment in extraction would reorient toward regimes requiring multilateral approval. The global resource economy in space would reorganize around equity and common-interest governance rather than first-mover enclosure.
% FOUNDING_PROBLEM: Ambiguity in Article II over whether private corporations could appropriate space resources without the nonappropriation principle being violated. Early space law prohibited sovereign territorial claims but was silent on private extraction. Spacefaring states interpreted the silence as permitting private ownership, resolving the commercial ambiguity in favor of capital formation and extraction ventures.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and extraction corporations attested the founding problem was real and resolved by the extraction-permissive reading, citing the need for investment clarity. Non-spacefaring states and conservation analysts counter that the founding problem was never resolved—it was answered by the most powerful parties rather than by legitimate interpretation. Scholarly and state-party testimony from the Global South and indigenous-peoples delegations explicitly contests whether the reading reflects Article II's intent, citing the 'province of all mankind' language in the preamble. No disinterested outside authority has validated the extraction-permissive reading as the correct interpretation; it persists via state practice and technology-backed fait accompli.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading allocates all resource value to spacefaring states and their corporations, with zero compensation to excluded parties. The allocation is decoupled from service cost or coordination benefit—it rides entirely on technological capability and state power. Suppression is substantial (0.71) because the reading's persistence depends actively on gatekeeping: spacefaring states maintain COPUOS voting control, suppress alternative interpretations through state practice and treaty-nonrevision, and deter non-spacefaring states from lodging formal objections by making exit costlier than continued compliance. Theater ratio (0.42) is moderate because the reading includes real coordination benefits (clarity on property rights, enabling investment), but an increasing share of enforcement activity (licensing review, regulation of extraction sites) defends the allocation to spacefaring beneficiaries rather than solving genuine coordination problems. Accessibility_collapse (0.68) reflects that alternatives (conservation reading, international-regime reading) are perceived as politically foreclosed for current extractors; non-spacefaring actors see the constraint as fait accompli, not as an open choice—but the collapse is not complete because COPUOS debates keep alternative framings alive at low amplitude. Resistance (0.54) is moderate: spacefaring states encounter real pushback in UN forums and scholarly literature, but enforcement is sufficient to hold the reading's practice. The measurement series tracks the constraint's trajectory from 1967 (legal ambiguity) through 2000 (de facto extraction beginning to normalize) to 2026 (commercial extraction ventures fully operational, reading entrenched). The rising extractiveness trend reflects accumulating fait accompli: each unchallenged extraction operation reinforces the extraction-permissive reading and makes reversal costlier. Theater_ratio rise reflects the gradual shift from 'clarifying an ambiguous treaty' (early narrative) to 'defending against conservation challenges' (current narrative)—enforcement machinery hardening around regulatory gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the spacefaring-state seat, the extraction-permissive reading is a legitimate, stabilizing interpretation that clarifies ambiguity and enables capital formation for space development—a genuine coordination mechanism. From the non-spacefaring-state seat, it is an enclosure regime that forecloses collective governance options and transforms the commons into private rentals extracted by the already-powerful. The engine computes these divergent classifications from the structural data (who benefits, who bears costs, what exits are available) and should show marked type differences across seats: spacefaring states compute toward rope or mild tangled_rope (coordination with modest asymmetry); non-spacefaring states compute toward snare or severe tangled_rope (asymmetric extraction with constrained exits and suppression). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring states and corporations occupy the beneficiary end (d near 0.0): they set rules, authorize extraction, and receive all proceeds. Non-spacefaring states occupy the target end (d near 1.0): they are excluded, pay the cost of forgone collective governance, and have constrained exit (identity-locked to UN participation). The international_court and conservation_advocates sit at the observer end, with no direct extraction proceeds but real structural interest in the reading's content. The extraction-permissive reading generates a sharp directionality gradient: high d for the powerless (excluded states), near-zero d for the institutional (spacefaring states), which is exactly the asymmetry that makes tangled_rope classification appropriate. The engine should compute this divergence from the structural data: spacefaring states benefit, non-spacefaring states pay; exit options differ drastically; power atoms position them at opposite ends of the extraction axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ambiguity over whether private extraction violates Article II) had a genuine coordination function in 1967–1985: clarifying legal boundaries for space investment. By 2000, extraction operations were demonstrably profitable and the founding problem had lost urgency for spacefaring states; the reading had transitioned from 'solution to ambiguity' to 'allocated entitlement.' By 2026, the founding problem is demonstrably dead (extraction is routine, not ambiguous; spacefaring states treat the reading as settled law), yet the reading persists as a vehicle for wealth extraction from excluded parties. Mandatrophy is partially resolved: the constraint meets the mandatrophy signature (founding problem dead, constraint persists, measured theater_ratio rising, suppression hardening around regulatory gatekeeping rather than coordination). However, the reading's persistence is actively maintained by spacefaring-state enforcement, not mere inertia—it is a snare or severe tangled_rope with institutional backing, not a piton. The theater_ratio rising trajectory (0.05 → 0.42) tracks the mandatrophy drift: early enforcement was thin (the reading was natural state practice), late enforcement is thick (active regulatory gatekeeping, COPUOS voting control, suppression of alternative interpretations). This is the signature of a reading transitioning from coordination to pure extraction, with theater_ratio as the temporal marker.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_textual_scope,
    'Does Article II''s ban on ''sovereignty or possession or any claim of sovereignty'' apply only to territorial claims or extend to de facto appropriation via resource extraction and private ownership?',
    'Treaty amendment (Article XII) or authoritative interpretation by the International Court of Justice or a new binding international regime (Article XI mechanism). Analysis of preparatory work (travaux préparatoires) from the 1967 treaty drafting may also narrow the ambiguity.',
    'If extended to extraction, the extraction-permissive reading collapses and the commons_conservation or international_regime reading becomes operative. Extracted resources would be governed by benefit-sharing or collective-stewardship frameworks, and backward-extraction claims from excluded states might achieve legal standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_ii_textual_scope, conceptual, 'Whether Article II''s non-appropriation principle covers resource extraction or only sovereign territorial claims.').

omega_variable(
    spacefaring_coalition_fracture,
    'Will spacefaring states maintain unified position on extraction-permissive reading, or will technology maturation and resource abundance fragment the coalition as capital costs fall and competition intensifies?',
    'Monitoring of flag-state regulatory divergence, emergence of competing licensing regimes, and voting patterns in COPUOS and General Assembly as new spacefaring actors (India, UAE, private spaceports) enter extraction markets.',
    'Coalition fracture would weaken suppression of the conservation reading; if middle-power spacefaring states shift to benefit-sharing frameworks to compete for developing-world partnership, the extraction-permissive reading loses state backing and transitions to snare or piton territory (defended by incumbent corporations and flag-state bureaucracy rather than living state consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spacefaring_coalition_fracture, empirical, 'Whether the spacefaring-state consensus sustains or fragments under pressure from new entrants and resource competition.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (economic barriers to launch, regulatory gatekeeping by spacefaring powers) or internalized (non-spacefaring states have accepted exclusion and no longer contest the reading, making enforcement theater)?',
    'Post-technology cascade: if developing-state launch costs fall below $50M per ton of orbital cargo, monitoring whether suppression persists (continued regulatory gatekeeping) or evaporates (cost-driven exit from the constraint). Also: COPUOS and General Assembly voting patterns—rising contestation or continued passive acceptance.',
    'If suppression is internalized and becomes theater, the constraint transitions from tangled_rope (enforced) to piton (performed); the moral authority of the extraction-permissive reading would deteriorate, enabling a transition to conservation or international-regime readings. If suppression is structural and technology does not lower barriers sufficiently, the extraction-permissive reading persists as enforceable tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative readings is maintained by gatekeeping or by internalized acceptance.').

omega_variable(
    commons_conservation_kernel_foreclosure,
    'Can the extraction-permissive reading logically coexist with the commons_conservation reading within a single legal framework, or do they foreclose each other?',
    'Formal legal analysis of whether ''non-appropriation applies to private actors (conservation reading)'' and ''non-appropriation applies only to states (extraction-permissive reading)'' can both be true under different interpretive methodologies, or whether one necessarily excludes the other. Test: can a court hold both readings without contradiction, or must it choose?',
    'If they foreclose each other, the engine flags the pair as a binary choice and one reading must eventually win through interpretive authority or state practice shift. If they coexist, both readings remain live even with different state backing, and the constraint landscape supports multiple reading-based interpretations of the same kernel simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_conservation_kernel_foreclosure, conceptual, 'Whether extraction-permissive and conservation readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1985, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1985, 0.12).
narrative_ontology:measurement_basis(ost__tr_t1985, projected).
narrative_ontology:measurement(ost__tr_t2000, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(ost__tr_t2000, observed).
narrative_ontology:measurement(ost__tr_t2013, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2013, 0.36).
narrative_ontology:measurement_basis(ost__tr_t2013, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(ost__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1985, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement_basis(ost__be_t1985, projected).
narrative_ontology:measurement(ost__be_t2000, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(ost__be_t2000, observed).
narrative_ontology:measurement(ost__be_t2013, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement_basis(ost__be_t2013, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(ost__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.22).
narrative_ontology:measurement_basis(ost__su_t1967, observed).
narrative_ontology:measurement(ost__su_t1985, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement_basis(ost__su_t1985, projected).
narrative_ontology:measurement(ost__su_t2000, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(ost__su_t2000, observed).
narrative_ontology:measurement(ost__su_t2013, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2013, 0.65).
narrative_ontology:measurement_basis(ost__su_t2013, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(ost__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% Article II of the Outer Space Treaty instantiates three structurally distinct constraints from a single kernel: the meaning of non-appropriation. The extraction-permissive reading (this file) treats non-appropriation as barring sovereign claims only, permitting private appropriation. The commons_conservation reading treats non-appropriation as covering de facto appropriation via extraction. The international_regime reading treats Article II as deferring the question to a future binding regime. Each reading has its own beneficiary/victim structure, ε, and classification. ε differs substantially: extraction-permissive is high-extractiveness (0.78) because the reading allocates all value to spacefaring parties; commons_conservation would be low-extractiveness (near 0.2) because the reading treats extraction as constrained by multilateral authorization; international_regime would be moderate-extractiveness (0.45) because extraction persists under current-state regulation pending regime creation. The three stories decompose a single contested kernel into three separate constraints via the ε-invariance principle: changing the interpretation changes what the constraint IS and how it distributes value. All three should be generated; they are linked via network.affects_constraints to show the kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
