% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number Entry: Hybrid Scaffolding Reading
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   Treating zero as an operable number was latent in positional notation
 *   from the moment place-value systems ran columns empty, but latency was
 *   not thinkability. Operational command, meaning rules for adding,
 *   subtracting, multiplying, and dividing by the empty place and eventually
 *   treating it as a quantity in its own right, required a conceptual
 *   scaffold licensing absence-as-manipulable. The Sanskrit intellectual
 *   ecosystem supplied one: Paninian grammar marked phonological absence with
 *   zero-equivalent operators, Buddhist Madhyamaka made shunyata a
 *   disciplined ontological category, Jaina cosmology normalized enumeration
 *   of the indefinitely small, and decimal astronomy demanded a working
 *   convention for empty columns. By Brahmagupta's Brahmasphutasiddhanta (628
 *   CE) the scaffold had produced codified zero-arithmetic. Transmission
 *   westward, through al-Khwarizmi's circle to Latin Europe via Fibonacci,
 *   did not deliver an alien finished concept so much as trigger recognition:
 *   European adopters with merchant fluency in multiple notations recognized
 *   what their own positional tools implied once shown a working instance.
 *   Traditions whose scaffolding was incompatible, Greek geometric algebra
 *   and its scholastic-Aristotelian inheritors, where number meant plurality
 *   of units and void was impossible, bore centuries of foregone capability,
 *   exiting their lock-in only as contact eroded the framework's exclusivity.
 *   The arrangement coordinates a shared conceptual vocabulary across
 *   calculating communities; its costs fall asymmetrically on the
 *   scaffold-incompatible, but nothing is transferred out of them and no seat
 *   administers or enforces it. KEY AGENTS (by structural relationship): -
 *   hindu_algebraic_tradition: primary beneficiary
 *   (organized/identity_locked) — held the compatible scaffold; first
 *   operational command - sanskrit_philosophical_grammarians: agenda-setter
 *   (organized/identity_locked) — built the scaffold licensing
 *   absence-as-manipulable - islamic_algebraic_tradition: secondary
 *   beneficiary and transmission broker (organized/constrained) -
 *   european_mathematical_recognizers: post-contact beneficiaries
 *   (moderate/arbitrage) — recognition triggered, conversion costs paid -
 *   greek_geometric_algebra_tradition: primary target of the asymmetry
 *   (institutional/trapped) — locked out by its own rigor -
 *   scholastic_aristotelian_metaphysicians: secondary target
 *   (institutional/identity_locked) - mayan_positional_tradition: excluded
 *   voice (powerless/trapped) — independent proof of scaffold-generality,
 *   absent from the record - historians_of_mathematics: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.42).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.12).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number Entry: Hybrid Scaffolding Reading").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, 'bceab93b-72da-4049-b7b1-f81bc04d44d1').
narrative_ontology:cs_kernel_codification('bceab93b-72da-4049-b7b1-f81bc04d44d1', distributed).
narrative_ontology:cs_authority_grounding('bceab93b-72da-4049-b7b1-f81bc04d44d1', distributed).
narrative_ontology:cs_reading_relation('bceab93b-72da-4049-b7b1-f81bc04d44d1', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('bceab93b-72da-4049-b7b1-f81bc04d44d1', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_axiom('bceab93b-72da-4049-b7b1-f81bc04d44d1', foundational, operational_thinkability_requires_compatible_scaffolding).
narrative_ontology:cs_axiom_status(operational_thinkability_requires_compatible_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('bceab93b-72da-4049-b7b1-f81bc04d44d1', operational_thinkability_requires_compatible_scaffolding, empirically_contingent).
narrative_ontology:cs_axiom('bceab93b-72da-4049-b7b1-f81bc04d44d1', foundational, contact_triggers_latent_structure_recognition).
narrative_ontology:cs_axiom_status(contact_triggers_latent_structure_recognition, holdable).
narrative_ontology:cs_axiom_grounding('bceab93b-72da-4049-b7b1-f81bc04d44d1', contact_triggers_latent_structure_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('bceab93b-72da-4049-b7b1-f81bc04d44d1', scaffold_conditional_operational_thinkability).
narrative_ontology:cs_drift_state('bceab93b-72da-4049-b7b1-f81bc04d44d1', contemporary_comparative_historiography, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('bceab93b-72da-4049-b7b1-f81bc04d44d1', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, european_mathematical_recognizers).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_metaphysicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, sanskrit_philosophical_grammarians).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, european_mathematical_recognizers).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, scaffolding_necessity_for_operational_thinkability).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, latent_availability_in_positional_notation).
narrative_ontology:constraint_vindicates(zero_as_number_entry__hybrid_scaffolding_reading, recognition_over_transmission_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From Aryabhata's positional astronomy through Brahmagupta's codified rules for operating on shunya (628 CE) to Bhaskara and the Kerala school, this community held grammatical, metaphysical, and computational resources that made treating the empty place as an operable quantity a natural extension of existing practice. The scaffold is fused with the tradition's wider intellectual identity; leaving it would mean leaving the practice. First-mover operational command of generalized arithmetic and algebra flowed to it.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition, beneficiary,
    organized, civilizational, identity_locked, regional).

% Paninian grammarians marking phonological absence with zero-equivalent operators, Buddhist Madhyamaka thinkers making shunyata a disciplined ontological category, and Jaina cosmologists normalizing enumeration of the indefinitely small built the conceptual machinery that mathematicians later instrumented. They set the terms of what could be thought about absence; they also collected confirmation of their frameworks' fertility when the machinery proved load-bearing for exact science.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, sanskrit_philosophical_grammarians, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, sanskrit_philosophical_grammarians, beneficiary).

% Al-Khwarizmi's circle, al-Karaji, and al-Samaw'al absorbed the Indian package, named it (sifr, the khwarazmian numerals), extended it into rhetorical algebra, and controlled the transmission channels westward through Baghdad, North Africa, and al-Andalus. They collected both the mathematical gains and the brokerage position; their practical utility in inheritance calculation, surveying, and commerce bound them to the arrangement.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, islamic_algebraic_tradition, agenda_setter).

% Merchant-trained mathematicians such as Fibonacci moved fluently between Roman numerals, counting-board reckoning, and Hindu-Arabic positional notation, adopting whichever served the transaction. Contact showed them a working instance and they recognized what their own positional tools implied. They gained the concept without having built the scaffold, but paid conversion costs: guild hostility, fraud accusations against cipher numerals, municipal bans such as Florence's 1299 prohibition, and the labor of translation.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, european_mathematical_recognizers, beneficiary,
    moderate, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__hybrid_scaffolding_reading, european_mathematical_recognizers, payer).

% Euclidean and Archimedean practice treated quantities as geometrical magnitudes; number meant plurality of units, unity was not a number, and void was a metaphysical impossibility. The framework's demonstrative rigor, its greatest strength, kept practitioners from treating nothing as an operable quantity. Leaving meant abandoning the standards that defined mathematical legitimacy itself, so the tradition bore centuries of foregone capability, visible only in retrospect.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_algebra_tradition, payer,
    institutional, civilizational, trapped, continental).

% University philosophers inherited the Greek ban on void as ontology: nothing comes from nothing, number is multitude, the category of quantity excludes privation-as-entity. When cipher numerals arrived they processed them through this framework, permitting placeholder use while denying number-status and suspecting fraud. The metaphysics constituted the rational defense of theology, so exit was identity-fused; the costs were delayed reconciliation and forced distinctions that eventually collapsed.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, scholastic_aristotelian_metaphysicians, payer,
    institutional, generational, identity_locked, continental).

% Mesoamerican calendar-keepers independently invented positional vigesimal notation with a zero sign centuries before sustained Old World contact, with no share in Eurasian scaffolds. Their achievement testifies that compatible scaffolding can arise elsewhere, but no transmission channel connected them to the debates that adjudicated the concept's history, and colonial destruction of their codices removed their testimony from the record until modern decipherment.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, mayan_positional_tradition, excluded,
    powerless, civilizational, trapped, continental).

% Modern scholars reconstruct the entry of zero from manuscripts, inscriptions such as the Gwalior zero, coinage, and transmission philology, including the contested dating of the Bakhshali manuscript. They weigh testimony from every surviving trace and their reconstructions shape how the history is taught, but they collect nothing from the arrangement and bear none of its historical costs.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__hybrid_scaffolding_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__hybrid_scaffolding_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_as_number_entry__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community of calculators on a shared conceptual vocabulary: conventions distinguishing placeholder-empty from quantity-zero, rules for operating on the empty place (codified in Brahmagupta's shunya arithmetic), and a common decimal positional encoding that lets results travel between practitioners. Once shared, arithmetic generalizes beyond particular magnitudes and algebra can detach from geometry.
% TRANSFER_FUNCTION: Moves operational capacity, meaning the ability to treat nothing as an operable quantity, from traditions holding compatible scaffolding to traditions gaining access through contact; before contact, it concentrated that capacity exclusively in scaffold-compatible traditions while imposing foregone-capability costs on scaffold-incompatible ones.
% ABSENT_VOICES: Independent positional-zero traditions, above all the Maya, were structurally outside the conversation and would testify that scaffold-compatible recognition does not require Indian-specific content. The Greek tradition could not articulate its own lock-in from inside it. Colonized American and African numerate communities were erased from the record that later adjudicated the question.
% DISAPPEARANCE_RATIONALE: If the scaffolding requirement vanished overnight, meaning operational thinkability needed no compatible conceptual scaffold, Greek mathematics absorbs zero in antiquity, the India-to-Islam-to-Europe transmission chain loses its explanatory role, the Florentine ban and abacist resistance never organize, and the historiography of mathematics reorganizes around a different question entirely. Every named seat's situation depends on the requirement holding.
% FOUNDING_PROBLEM: Making positional computation reliable where columns run empty: astronomers and merchants needed a convention for the empty place before anyone needed zero as a quantity, and the scaffold that solved the placeholder problem turned out to license the number.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: numerical-cognition research on why learners require external representational scaffolds, comparative historiography of Chinese rod-numeral, Babylonian placeholder, and Maya zero development showing independent scaffold constructions, and modern pedagogy research showing the same scaffold-dependence recurring for negative numbers, imaginary numbers, and limits. No beneficiary attests alone.
narrative_ontology:disappearance_verdict(zero_as_number_entry__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__hybrid_scaffolding_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).
:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the asymmetry is real but non-transferential: scaffold-incompatible traditions bore foregone capability, yet no value flowed out of them to any seat, and once the scaffold was shared the benefit pool expanded without anyone paying in. Suppression is low at end state (0.12) and is framework-immanent rather than enforced: the barrier was internalized metaphysical commitment, not external coercion, and it declined monotonically as contact offered external contrast; the suppression_requirement series is authored because the story's central dynamic is precisely the erosion of that barrier force under transmission. Theater is minimal (0.14) because nothing performs compliance here; the slight rise tracks historiographic mythologization (lone-genius Fibonacci legends, the misattributed name 'Arabic numerals'), not institutional ritual. Accessibility_collapse is moderate (0.55): once the scaffolding requirement is understood, the alternative account in which pure logical availability suffices for thinkability largely collapses, but sibling framings of the kernel remain coherently holdable. Resistance is substantial (0.52): the historical record shows organized opposition (Florence's 1299 ban, abacist guilds, scholastic fraud suspicions) plus ongoing historiographic contest. Claim and metrics are independent authored facts: the type is claimed as rope because the arrangement solves a genuine coordination problem (shared conceptual vocabulary) with no coercive overhead and no rent collection, while the metrics honestly register the asymmetric opportunity costs; where the computed per-seat types diverge from the claim, that divergence is the datum. All three metric series run on one shared eight-point grid (628-1650) so no end-state value is injected into earlier times. The single uptick in extractiveness at 1299 reflects institutional resistance widening the access gap, not cyclical oscillation. Receipt surface: gain_flow is authored as diffuse after checking every seat, because the asymmetry's cost is foregone possibility that accrues to no one; fixing_cost is prohibitive because no seat could dissolve the scaffolding requirement at acceptable cost, it eroded only through centuries of civilizational contact. Although prohibitive-plus-diffuse is the cell associated with degraded arrangements, this is not one: the coordination function is live, theater is minimal, and persistence reflects continued descriptive accuracy rather than inertia.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From inside the Hindu algebraic tradition the scaffold is liberating infrastructure it grew up with, experienced as pure enablement. From inside the Greek and scholastic seats the same conceptual economy operated as an inaccessible wall whose opening required surrendering the framework that conferred mathematical or theological legitimacy. The recognizer seat experiences it as arbitrage windfall with conversion friction. The engine computes these divergent per-seat classifications from power, exit, and declared position; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the Hindu algebraic tradition (subsidized by its own scaffold, identity-fused), the Islamic tradition (gains plus brokerage), and the European recognizers (arbitrage-grade exit pushes them nearest the beneficiary end despite conversion costs). Victim declarations drive high directionality for the Greek geometric algebra tradition (trapped: exit meant abandoning demonstrative legitimacy itself) and the scholastic metaphysicians (identity_locked: the framework constituted their worldview). The Maya seat is excluded rather than positioned: it feeds the consensus-provenance check as testimony, not the directionality computation. The critical interpretive point is that the victims' costs are counterfactual (foregone capability) rather than transfers; the omega variable victim_status_opportunity_cost documents the resulting rope-versus-tangled-rope pressure rather than resolving it by fiat.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandate and no administrator: no seat sets, enforces, or maintains the scaffolding requirement, so mandatrophy_resolved is not declared and no lifecycle-toward-piton dynamics are authored. The rope claim prevents misreading the arrangement as pure extraction, since nobody collects rents through it and the benefit pool expands without corresponding payments; the victim declarations prevent misreading it as costless harmony, since real opportunity costs fell on the scaffold-incompatible for centuries. Persistence is explained by continued descriptive accuracy (every subsequent formalism, from negatives to infinitesimals, re-runs the scaffold problem) rather than by institutional inertia or theatrical maintenance, which is why theater_ratio stays low across the whole interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_zero_entry,
    'This constraint instantiates one reading (hybrid_scaffolding_reading) of the zero_as_number_entry kernel; how would the sibling readings restructure it?',
    'Comparative compilation of the sibling stories: contingent_thinkability_reading raises epsilon toward the metaphysical-barrier pole and expands the target set to all non-transmitting traditions; universal_discovery_reading drives epsilon toward negligible and dissolves the target set entirely.',
    'If the universal reading prevails, the scaffolding requirement becomes an epistemic footnote and the constraint decomposes toward unconditional availability; if the contingent reading prevails, the target set expands and suppression rises. This story''s moderate profile depends on the hybrid middle holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_zero_entry, conceptual, 'Committer structure: one of three readings of the zero-entry kernel; siblings change the victim set and epsilon.').

omega_variable(
    scaffold_specificity_vs_genericity,
    'Was the operative scaffold the specific Indian philosophical complex (shunyata, Paninian markers, Jaina enumeration) or would any compatible scaffold have sufficed?',
    'Natural experiments: Maya vigesimal zero (independent scaffold, successful recognition) versus Chinese rod-numeral practice (different scaffold, placeholder competence without number-status until very late). Compare recognition outcomes across independent scaffold constructions.',
    'If generic scaffolds suffice, the Greek seat''s locked-out position is contingent on its framework''s specific rigidity and epsilon falls; if Indian-type scaffolds were unusually fertile, the beneficiary concentration is structural and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_specificity_vs_genericity, empirical, 'Whether the scaffolding contingency attaches to the specific Indian complex or to scaffold-compatibility generally.').

omega_variable(
    transmission_vs_recognition_disentanglement,
    'Did contact transmit a packaged concept or trigger recognition of a latent structure, and can manuscript evidence distinguish the two?',
    'Philology of the transmission texts (Liber Abaci''s sources, the Latin recensions of al-Khwarizmi): re-derivation and error-correction by adopters indicates recognition; verbatim rule-copying with persistent misapplication indicates transmission. Cross-check against how quickly characteristic error patterns disappear in European manuscripts.',
    'Demonstrated recognition supports this reading''s latent-structure claim and keeps epsilon moderate; pure transmission shifts weight toward the contingent reading''s emphasis on contact as necessary carrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_vs_recognition_disentanglement, empirical, 'Mechanism ambiguity at the heart of the hybrid reading.').

omega_variable(
    victim_status_opportunity_cost,
    'Do scaffold-incompatible traditions count as targets bearing extraction, or merely as non-participants who never interfaced with the structure?',
    'Boundary analysis: extraction properly requires costs flowing through the arrangement''s own operation. Foregone capability is a counterfactual cost, not a transfer; examine whether the engine''s effective-extraction computation treats counterfactual costs as extractive flow.',
    'If counterfactual costs count, the rope classification carries permanent tangled_rope pressure; if not, the victim declarations function as directional markers and the rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_opportunity_cost, conceptual, 'Whether opportunity-cost targets are extraction targets or directional markers only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 628, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_entry_hybrid_tr_t628, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 628, 0.06).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t628, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t825, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 825, 0.07).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t825, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t976, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 976, 0.08).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t976, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t1202, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1202, 0.09).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t1202, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t1299, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1299, 0.1).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t1299, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t1489, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1489, 0.11).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t1489, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t1545, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1545, 0.13).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t1545, observed).
narrative_ontology:measurement(zero_entry_hybrid_tr_t1650, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 1650, 0.14).
narrative_ontology:measurement_basis(zero_entry_hybrid_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(zero_entry_hybrid_be_t628, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 628, 0.5).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t628, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t825, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 825, 0.47).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t825, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t976, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 976, 0.46).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t976, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t1202, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1202, 0.45).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t1202, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t1299, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1299, 0.46).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t1299, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t1489, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1489, 0.44).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t1489, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t1545, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1545, 0.43).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t1545, observed).
narrative_ontology:measurement(zero_entry_hybrid_be_t1650, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 1650, 0.42).
narrative_ontology:measurement_basis(zero_entry_hybrid_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_entry_hybrid_su_t628, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 628, 0.55).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t628, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t825, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 825, 0.49).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t825, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t976, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 976, 0.46).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t976, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t1202, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1202, 0.4).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t1202, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t1299, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1299, 0.36).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t1299, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t1489, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1489, 0.27).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t1489, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t1545, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1545, 0.18).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t1545, observed).
narrative_ontology:measurement(zero_entry_hybrid_su_t1650, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 1650, 0.12).
narrative_ontology:measurement_basis(zero_entry_hybrid_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, universal_discovery_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the discovery of zero' decomposes into three structurally distinct readings of one kernel (zero_as_number_entry): contingent_thinkability_reading (transmission-necessity, metaphysical-barrier pole), this hybrid_scaffolding_reading (latent availability plus scaffold-conditioned thinkability, moderate on both contingency and necessity), and universal_discovery_reading (logical availability, holder-priority irrelevant). Each carries its own epsilon, beneficiary/victim structure, and classification; this file authors only the hybrid reading. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
