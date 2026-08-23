% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Persistence as Path-Dependent Coordination Failure (Lock-In Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   A single character-to-key layout invented for 1870s mechanical
 *   typewriters governs virtually all text input on Earth. Its founding
 *   function — keeping metal typebars from wedging — became physically
 *   meaningless with electronic switching, yet the mapping persists through
 *   nothing stronger than reproduced habit: factories, curricula,
 *   certification tests, and the motor memory of hundreds of millions. This
 *   story generates the lock_in_reading of the qwerty_persistence_mechanism
 *   kernel as a clean, epsilon-invariant constraint over one referent: the
 *   standing arrangement of universal adoption of the incumbent layout,
 *   assessed by this reading's own lights. KEY AGENTS (by structural
 *   relationship): see key_agents below. The claimed type (piton) is authored
 *   independently of the metrics; the metrics describe the arrangement's
 *   actual operation — modest diffuse deadweight, near-zero coercion, partial
 *   social nonviability of alternatives. Sibling files carry the other two
 *   readings of the same kernel; the family decomposition is documented in
 *   network.dual_formulation_note and the committer structure in
 *   commentary.kernel_context and omegas.
 *
 * KEY AGENTS:
 *   - - os_and_firmware_vendors: de facto agenda-setter (institutional/arbitrage) — controls shipped defaults, could change them unilaterally, bears and collects nothing specific to the mapping
 *   - - keyboard_hardware_manufacturers: incidental beneficiary (institutional/mobile) — collects standardization value, indifferent to which layout
 *   - - typing_education_and_certification_providers: incidental beneficiary (organized/constrained) — sells instruction denominated in the incumbent layout
 *   - - qwerty_trained_workforce: principal bearer (moderate/constrained) — pays diffuse efficiency costs; its sunk skill simultaneously sustains the standard
 *   - - high_volume_text_professionals: concentrated bearer (moderate/constrained) — compounds per-keystroke losses into material annual totals
 *   - - dvorak_alternative_layout_advocates: excluded challenger (powerless/trapped) — holds a ready alternative no channel will move first on
 *   - - technology_historians_path_dependence_scholars: analytical observer — attests the genealogy and contests the magnitude claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.35).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.15).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Persistence as Path-Dependent Coordination Failure (Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '5838dafc-afac-4e39-9e2c-fe00853d1863').
narrative_ontology:cs_kernel_codification('5838dafc-afac-4e39-9e2c-fe00853d1863', formalized).
narrative_ontology:cs_authority_grounding('5838dafc-afac-4e39-9e2c-fe00853d1863', practice).
narrative_ontology:cs_interpretation_layer_present('5838dafc-afac-4e39-9e2c-fe00853d1863').
narrative_ontology:cs_reading_relation('5838dafc-afac-4e39-9e2c-fe00853d1863', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('5838dafc-afac-4e39-9e2c-fe00853d1863', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5838dafc-afac-4e39-9e2c-fe00853d1863', foundational, persistence_requires_no_active_maintainer).
narrative_ontology:cs_axiom_status(persistence_requires_no_active_maintainer, holdable).
narrative_ontology:cs_axiom_grounding('5838dafc-afac-4e39-9e2c-fe00853d1863', persistence_requires_no_active_maintainer, empirically_contingent).
narrative_ontology:cs_axiom('5838dafc-afac-4e39-9e2c-fe00853d1863', foundational, private_switching_costs_exceed_private_gains).
narrative_ontology:cs_axiom_status(private_switching_costs_exceed_private_gains, holdable).
narrative_ontology:cs_axiom_grounding('5838dafc-afac-4e39-9e2c-fe00853d1863', private_switching_costs_exceed_private_gains, empirically_contingent).
narrative_ontology:cs_reference_frame('5838dafc-afac-4e39-9e2c-fe00853d1863', remington_1873_de_facto_freeze).
narrative_ontology:cs_drift_state('5838dafc-afac-4e39-9e2c-fe00853d1863', contemporary_electronic_input_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5838dafc-afac-4e39-9e2c-fe00853d1863', '2026-07-28T12:00:00Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typing_education_and_certification_providers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_professionals).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, dvorak_alternative_layout_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship a default character-to-key mapping in every operating system and device firmware, and provide utilities that remap it. Changing the shipped default is technically trivial; the reason it does not happen is that no customer segment demands it loudly enough to outweigh support costs and disruption complaints. They neither charge nor collect anything tied to the particular mapping; they administer whichever mapping the installed base already knows.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, os_and_firmware_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Mold keycaps and assemble boards for whatever layout distribution channels stock. A stable worldwide mapping simplifies tooling, inventory, and compatibility testing regardless of which mapping it is; they would tool a production line for a rival layout as soon as orders appeared. Their interest attaches to standardization as such, not to the particular letters printed on the caps.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Sell courses, textbooks, drill software, and speed certificates keyed to the mapping nearly all learners arrive with. Curricula and assessment benchmarks are written against the incumbent layout; a wholesale migration would strand lesson materials and test norms for a few years, after which they would teach whatever became standard. Tuition flows to them under any dominant layout alike.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_education_and_certification_providers, beneficiary,
    organized, biographical, constrained, national).

% Hundreds of millions of people carry typing fluency stored as motor memory in the incumbent layout, acquired at real cost in school or on the job. Every document typed sustains the mapping's universality; every hour carries a small efficiency penalty invisible at the individual scale. Learning a rival layout means weeks or months of depressed output and losing fluency on every machine but one's own, so almost no one starts; collectively they are the majority who would gain from a coordinated move, yet each keeps typing.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, qwerty_trained_workforce, beneficiary).

% Transcriptionists, data-entry operators, court reporters, and heavy correspondence staff spend entire working days at the keyboard, compounding any per-keystroke inefficiency into material annual totals. Employers hire and benchmark them on speed tests written for the dominant layout, and shared workstations penalize anyone whose fingers expect different letters. Retraining one desk is affordable; retraining a department, with interim output loss, has rarely been attempted.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, high_volume_text_professionals, payer,
    moderate, biographical, constrained, global).

% Designers and promoters of rival mappings (the best-known patented in 1936 and refined since) argue their arrangements place common letters on the home row and reduce finger travel. They publish comparisons, sell kits, and petition employers and agencies, but adoption requires simultaneous movement by trainers, employers, and hardware channels, none of whom moves first. Their proposals remain fully available to anyone willing to go alone.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, dvorak_alternative_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Reconstruct how the 1870s mechanical design became universal, and test claims about its present-day cost against archival evidence and replication studies. They sit outside every commercial and pedagogical seat, and the durability of their disagreement — over how large the efficiency penalty really is, and whether adoption was ever genuinely path-dependent — is itself evidence about which parts of the story are settled.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, technology_historians_path_dependence_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interoperability problem that every typist's motor memory, every keyboard's keycaps, every instructional curriculum, and every hiring speed-test must agree on one character-to-key mapping. A shared layout is a genuine public good and the arrangement supplies one; the open question this reading brackets is which mapping, and at what efficiency cost.
% TRANSFER_FUNCTION: Transfers essentially nothing between parties: the arrangement's principal cost is destroyed, not captured — aggregate typing-time inefficiency dissipates as lost output with no receiving seat. Incidental flows move tuition to instruction providers and standardization convenience to hardware makers, both of which any stable layout would generate equally.
% ABSENT_VOICES: Rival-layout designers and ergonomics researchers would object that superior mappings were never trialed at scale under fair conditions; newly trained generations of typists, who inherit the layout without consent, have no seat in any standards process; and no deliberative body exists at all — the decision was made cumulatively by adoption, so dissent had no room to register.
% DISAPPEARANCE_RATIONALE: Keyboards, firmware defaults, school curricula, employment tests, and several hundred million people's motor memory are built on the mapping; overnight disappearance would force emergency remapping and retraining on a planetary scale. Long-run the world would likely settle on a comparable layout at similar efficiency — the rearrangement would be enormous and the permanent difference small, which is precisely the signature this reading attributes to the arrangement.
% FOUNDING_PROBLEM: Mechanical typebar jamming in 1870s typewriters: adjacent frequent letter pairs struck in quick succession wedged the swinging typebars. Christopher Sholes' layout separated common English pairs to let bars clear, and the Remington commercialization of 1873 froze the design before any efficiency comparison against alternatives existed.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside every benefiting seat: the technology-history literature descending from Paul David's 1985 paper reconstructs the jamming rationale from patent records and period sources; keyboard-engineering references confirm electronic switching made typebar collision physically impossible decades ago; and no hardware maker or instruction provider currently defends the arrangement on jamming grounds — they cite only user familiarity, which itself corroborates that the founding problem no longer motivates maintenance.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.35: the arrangement levies a real but bounded efficiency toll on text production, and — the defining feature of this reading — no seat collects it; the value dissipates as lost output. Suppression is low (0.15) because nothing forbids alternatives: operating systems ship remapping utilities, rival boards are purchasable, self-teaching materials are abundant; what binds is the private cost calculus, not coercion. Accessibility collapse is moderate (0.60): alternatives remain individually open but socially nonviable — solo adopters forfeit shared-machine compatibility, employment benchmarks, and colleague legibility — so the option set collapses for collective choice while surviving for individuals. Resistance is low (0.20): advocacy flared during the 1930s-40s conversion campaigns and decayed after repeated adoption reversions; per-person stakes are too small to organize around, which is the Olson condition this reading asserts. Theater ratio 0.32: a minority of maintenance activity is performative (origin folklore in typing pedagogy, efficiency marketing that never faces comparison), but the bulk of persistence is genuine infrastructural reproduction — factories, curricula, and motor memory doing ordinary work. The temporal series share one grid (t=0..150, step 15) across both tracked metrics. No suppression_requirement series is authored: the enforcement picture is static at approximately zero — there is no enforcement machinery — and the scalar suppression value carries that fact. The series show extraction accumulating after the founding function dies (t~90) and plateauing as input-method diversification dilutes per-user stakes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the OS-vendor seat the arrangement reads as neutral infrastructure: changeable in an afternoon, unmourned, unrevenue-generating. From the workforce seat it reads as an inherited endowment that is simultaneously the tax and the treasury — each typist's sunk skill is the enforcement substrate, so the largest payer class reproduces the arrangement that taxes it every time it touches a keyboard. From the advocate seat it reads as a closed door: a ready alternative no channel will move first on. Same-power lateral divergence: two institutional seats (OS vendors versus hardware makers) hold equal power atoms but opposite structural relationships — one administers defaults indifferently, one collects standardization convenience — and two moderate seats (general workforce versus high-volume professionals) differ only in loss intensity, not in any lever either can pull. Coalition potential among payers is exactly the coordination this reading says fails: typists are the overwhelming majority and could vote the mapping aside within a generation, yet each rationally keeps typing, which is why no coalition forms.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: hardware manufacturers and instruction providers enter as beneficiaries with mobile/constrained exits, pulling their effective extraction toward the subsidy end; the general workforce enters as payer with secondary-beneficiary standing and constrained exit — its directionality lands target-side but damped by the sunk-capital benefit it would forfeit in any migration; high-volume professionals enter as pure payers with the same constrained exit, landing nearer the full-target end; rival-layout advocates enter as payers with trapped exit, effectively full targets of a door that will not open; OS vendors enter as agenda-setter with neither declared gain nor declared loss, landing near symmetric. No directionality_overrides are authored: the derivation chain already separates the two institutional seats through their differing role and exit declarations, and a power-atom-keyed override would collide them (both institutional) despite opposite true relationships — the coarse override key cannot express what the structural data already encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping mechanical typebars from wedging — died with electronic switching near t=90 on the authored grid, and the arrangement persisted untouched. The R5 interview records founding_problem_status dead alongside disappearance_verdict world_rearranges; the mismatch flags the arrangement as a zombie candidate, and the cross-check lands exactly on the inertial profile: what persists is not captured value (no seat receives the extraction — hence gain_flow diffuse) but reproduced habit — curricula, tooling, motor memory. The classification prevents mislabeling in both directions: read as a voluntary standard, the arrangement would look like something people keep choosing, which the record contradicts; read as captured extraction, it would require a profiting maintainer, whom the record does not contain; the inertial classification locates it as a former coordination solution running on after its function expired, with diffuse costs no seat internalizes and no seat motivated to repair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the lock_in_reading of the qwerty_persistence_mechanism kernel; how would the sibling readings (naturalization_reading, beneficiary_extraction_reading) restructure the same referent?',
    'Compile and compare the three sibling stories over the identical referent: the naturalization reading should drive epsilon toward the information-standard coordination floor and empty the victim set; the extraction reading should add concentrated capturer seats and raise suppression sharply; the disagreement is located in (a) whether a real efficiency deficit exists and (b) whether persistence requires active maintainer behavior.',
    'If naturalization wins, this reading''s epsilon collapses toward the coordination floor and its classification dissolves toward benign standard; if extraction wins, gain flow names specific seats and the classification hardens toward captured forms; this reading survives only in the middle case of inertia without capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega recording that this file is one of three readings of the QWERTY kernel and where the readings diverge structurally.').

omega_variable(
    dvorak_advantage_magnitude,
    'How large is the real-world efficiency deficit of the incumbent layout relative to optimized alternatives such as Dvorak?',
    'Controlled retraining studies with matched practice hours, meta-analysis of the contested naval-era trials, and instrumented keystroke analyses of contemporary professional populations.',
    'A near-zero deficit dissolves this reading into the naturalization sibling; a deficit in the range the early studies claimed makes the aggregate deadweight material and strengthens the case for coordinated migration economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_magnitude, empirical, 'Magnitude of the technical-inferiority claim underlying the lock-in account.').

omega_variable(
    counterfactual_fair_competition_path,
    'Would a superior layout plausibly have displaced the incumbent absent inherited installed base — i.e., is this a coordination failure, or simply an unremarkable equilibrium among roughly equivalent designs?',
    'Historical natural experiments: firm-level and government migrations that did occur (wartime retraining programs, dedicated-agency conversions), tracked for persistence and measured productivity deltas; comparative adoption paths of later input methods that were layout-independent from birth.',
    'If attempted migrations consistently reverted even with subsidized training, persistence reflects rational preference and this reading weakens toward naturalization; if subsidized cohorts held their gains but diffusion stalled at organizational boundaries, the coordination-failure mechanism is confirmed at the boundary identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_fair_competition_path, empirical, 'Whether the counterfactual without inherited installed base would have selected differently.').

omega_variable(
    migration_cost_financing_gap,
    'Does any financing mechanism exist under which aggregate retraining cost is smaller than aggregate discounted benefit, and could any single seat fund it?',
    'Cost accounting of employer-funded retraining pilots (wages during the learning dip, error-correction overhead, dual-layout transition hardware) against measured productivity deltas at plausible discount rates and agreed social weightings.',
    'A financeable transition would convert the arrangement from inertial residue into a neglected upgrade and pull classification away from inertia toward neglect; absence of any feasible funder confirms the cost-asymmetry that defines the inertial form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(migration_cost_financing_gap, preference, 'Whether the collective surplus is reachable by any feasible financing path — resolution depends on chosen discount rates and social weights, not only on measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(qwer_tr_t15, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(qwer_tr_t45, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 45, 0.12).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(qwer_tr_t90, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 90, 0.24).
narrative_ontology:measurement(qwer_tr_t105, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 105, 0.27).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement(qwer_tr_t135, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 135, 0.31).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 150, 0.32).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t15, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(qwer_be_t45, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 45, 0.13).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 75, 0.2).
narrative_ontology:measurement(qwer_be_t90, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 90, 0.23).
narrative_ontology:measurement(qwer_be_t105, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 105, 0.27).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 120, 0.31).
narrative_ontology:measurement(qwer_be_t135, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 135, 0.34).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 150, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__lock_in_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'QWERTY persists' decomposes per the epsilon-invariance principle into three readings of one kernel, each with a distinct epsilon over the SAME referent (universal adoption of the incumbent character-to-key mapping). naturalization_reading authors epsilon near the coordination floor (adequacy; rivals lapsed fairly); this lock_in_reading authors moderate diffuse epsilon with no capturing seat (deadweight inefficiency sustained by inertia); beneficiary_extraction_reading authors high epsilon with named capturer seats (incumbents defending training investments). Edge structure: the naturalization claim (fair competition, adequacy) is the evidence extraction-skeptics cite against lock-in, running upstream; the lock-in and extraction readings share acceptance of path dependence and differ only on agency, running laterally. Each family member links the other two via affects_constraints; no member is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
