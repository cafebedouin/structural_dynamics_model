% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Classical Latin Standard — Continuity Reading (Correctness as Unbroken Living Practice)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTINUITY READING of the
 *   classical_latin_standard kernel: correct Latin is the form carried by
 *   unbroken communal practice, and natural drift along the way is legitimate
 *   development, not corruption. The arrangement operated across Latin Europe
 *   from the Carolingian reforms to the threshold of the Renaissance:
 *   schools, chanceries, liturgy, and the learned professions formed users in
 *   living use, and correctness was adjudicated by formed ears rather than by
 *   textual archaeology. Its genuine coordination achievement is a working
 *   pan-European learned medium; its costs are a front-loaded formation toll,
 *   a lineage premium that discounts equally competent outsiders, and a thin
 *   injured fringe of under-formed performers and textual workers whose
 *   products are marked down regardless of merit. Per the epsilon-invariance
 *   principle, the colloquial label 'correct Latin' decomposes into three
 *   structurally distinct claims — this continuity reading, a reconstruction
 *   reading grounding correctness solely in recovered Classical texts, and a
 *   hybrid reading requiring both — authored as separate stories and linked
 *   through network edges; each carries its own epsilon, beneficiaries, and
 *   victims. The claim/metrics split is deliberate: the arrangement is
 *   CLAIMED as rope (genuine coordination, drift legitimized, no systematic
 *   delegitimization of alternatives) while the authored metrics record
 *   honestly moderate extraction concentrated on the least-formed seats; the
 *   engine computes per-seat classifications from the structural data, and
 *   any divergence between claim and computation is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - practice_lineage_masters: agenda-setting beneficiary (institutional / identity_locked) — transmit, certify, and adjudicate correctness; their authority IS the transmission chain
 *   - formed_clerical_scholarly_users: primary beneficiary (organized / constrained) — collect the working pan-European medium their formation purchased
 *   - aspiring_formed_students: dual seat (moderate / constrained) — pay formation costs up front, receive access and standing only after
 *   - underformed_parish_clergy: minimal victim (powerless / trapped) — perform required functions in a language they incompletely command, with no voice in the criteria
 *   - textual_fidelity_reformers: minimal victim (moderate / mobile) — scrupulous textual work persistently marked down for lacking formation lineage
 *   - vernacular_intellectuals: excluded voice (moderate / mobile) — demonstrate the medium is replaceable while holding no seat in the standard-setting conversation
 *   - modern_comparative_philologists: analytical observer — measure transmitted practice against reconstructed Classical norms across the whole interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.42).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.24).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Classical Latin Standard — Continuity Reading (Correctness as Unbroken Living Practice)").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '43237e7e-d9b8-4abf-bb33-688f928a1b4a').
narrative_ontology:cs_kernel_codification('43237e7e-d9b8-4abf-bb33-688f928a1b4a', distributed).
narrative_ontology:cs_authority_grounding('43237e7e-d9b8-4abf-bb33-688f928a1b4a', practice).
narrative_ontology:cs_interpretation_layer_present('43237e7e-d9b8-4abf-bb33-688f928a1b4a').
narrative_ontology:cs_reading_relation('43237e7e-d9b8-4abf-bb33-688f928a1b4a', classical_latin_standard__reconstruction_reading, forecloses).
narrative_ontology:cs_reading_relation('43237e7e-d9b8-4abf-bb33-688f928a1b4a', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('43237e7e-d9b8-4abf-bb33-688f928a1b4a', foundational, unbroken_transmission_constitutes_correctness).
narrative_ontology:cs_axiom_status(unbroken_transmission_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('43237e7e-d9b8-4abf-bb33-688f928a1b4a', unbroken_transmission_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('43237e7e-d9b8-4abf-bb33-688f928a1b4a', foundational, linguistic_drift_is_legitimate_development).
narrative_ontology:cs_axiom_status(linguistic_drift_is_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('43237e7e-d9b8-4abf-bb33-688f928a1b4a', linguistic_drift_is_legitimate_development, empirically_contingent).
narrative_ontology:cs_reference_frame('43237e7e-d9b8-4abf-bb33-688f928a1b4a', unbroken_practice_transmission).
narrative_ontology:cs_drift_state('43237e7e-d9b8-4abf-bb33-688f928a1b4a', renaissance_humanist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('43237e7e-d9b8-4abf-bb33-688f928a1b4a', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, practice_lineage_masters).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, formed_clerical_scholarly_users).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, underformed_parish_clergy).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, textual_fidelity_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, aspiring_formed_students).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, aspiring_formed_students).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, unbroken_transmission_legitimacy).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, custom_governs_written_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grammar masters, chancery-trained secretaries, and seminary and arts-faculty teachers who transmit correct Latin by forming students in use rather than from texts alone. They hear and correct composition, certify notaries, clerks, and graduates, and their own standing consists entirely in being recognizable links in an unbroken chain of formation. Leaving the practice would forfeit the only credential they hold; their authority and their identity are the same thing.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, practice_lineage_masters, agenda_setter,
    institutional, generational, identity_locked, continental).

% Clergy, canonists, physicians, and scholar-administrators formed in the schools who read and compose Latin daily. The living standard gives them a working medium usable from Ireland to Sicily without translation, and their competence is recognized on sight because it descends from the same formation their counterparts received. Their careers are built inside the medium; stepping out means retraining in vernacular or technical registers.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, formed_clerical_scholarly_users, beneficiary,
    organized, generational, constrained, continental).

% Boys and young men entering cathedral schools, arts faculties, and notarial training who pay years of fees and labor to acquire the practiced idiom. Until formed, their Latin is marked as provincial or barbarous regardless of how much they have read; after formation they join the ranks of recognized users. The payment is front-loaded and borne before any of the benefits arrive, and the career paths open to them assume the formation has happened.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, aspiring_formed_students, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, aspiring_formed_students, payer).

% Rank-and-file priests with thin schooling who must recite the liturgy, administer sacraments, and draft routine documents in a Latin they only partially command. Errors draw correction from superiors; the duties of their office cannot be performed in any other language; and no mechanism exists by which their experience of the burden reaches the people who set what counts as correct.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, underformed_parish_clergy, payer,
    powerless, biographical, trapped, regional).

% Learned writers and copyists whose work is grounded in close comparison of ancient manuscripts rather than in formation within the living practice. Under the operative criterion their productions are discounted — pronounced artificial, pedantic, or defective — however scrupulous the textual work behind them, because standing attaches to lineage of formation rather than to documented fidelity to ancient sources. They can publish, travel, and seek patrons elsewhere, but within the practice's jurisdiction their work carries a persistent mark against it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, textual_fidelity_reformers, payer,
    moderate, biographical, mobile, continental).

% Poets, chroniclers, and officials writing in French, Italian, Castilian, and the other rising vernaculars, together with the lay audiences they serve. They stand outside the formation system entirely — most are barred from it by station or sex — and their growing bodies of work demonstrate that learned administration and high literary art can proceed in other media. They have no seat in any discussion of what correct Latin is, yet the spread of their alternative steadily changes what the Latin standard is worth.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_intellectuals, excluded,
    moderate, generational, mobile, continental).

% Later scholars who reconstruct Classical phonology, morphology, and usage from inscriptions, manuscripts, and comparative Romance evidence, and who can therefore measure, century by century, how far transmitted practice drifted from the Classical norm and whether that drift preserved authentic development or accumulated corruption. They bear no costs and collect no benefits under the arrangement; they can see its whole arc.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, modern_comparative_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, practice_lineage_masters).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single working learned language across a fragmented continent and across generations: formation in living practice transmits pronunciation, idiom, rhythm, and compositional habit that written texts alone cannot carry, allowing clergy, jurists, physicians, and scholars to read and compose interchangeably from York to Palermo without translation.
% TRANSFER_FUNCTION: Moves years of formation-time, schooling fees, and public deference from aspirants and under-formed users to the masters and schools that control admission to the practice; moves certification of correctness — and the professional standing attached to it — to whoever can document descent from the transmission chain.
% ABSENT_VOICES: Under-formed parish clergy live the standard as a duty performed in a half-understood tongue but have no seat where its criteria are set; laypeople and women, largely barred from Latin formation altogether, bear the exclusion's costs wholly outside the conversation; vernacular intellectuals who argue the learned language itself is the problem are heard only as adversaries at the gate, never as participants in setting the standard.
% DISAPPEARANCE_RATIONALE: If the living-practice standard vanished overnight, cross-border clerical administration, legal procedure, medical teaching, and scholarly correspondence would fragment into mutually hardening regional Latins or collapse into dependence on translation; schools, chanceries, and liturgy would have to reorganize around explicit codified rules or around vernaculars decades before print and humanist schooling made either feasible at scale.
% FOUNDING_PROBLEM: After the breakdown of Roman imperial administration, educated communication across politically fragmented Europe needed a supralocal medium; keeping one alive required transmitting it through continuously functioning institutions — singing, dictating, correcting — rather than through dead texts that no one could pronounce with confidence.
% FOUNDING_PROBLEM_CORROBORATION: Demand-side users corroborate the problem's persistence from outside the master-beneficiary set: municipal governments hiring and paying notaries, princely chanceries staffing correspondence, medical faculties examining candidates in Latin, and church councils conducting business in it — all attest that a common learned medium was still needed and still in use through the end of the interval. No arbiter fully external to the Latin-using learned world exists to attest it, since the problem's very statement presupposes that world; but the paying consumers of the medium are not its beneficiaries, and they kept buying it.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 (moderate): the arrangement charges real, recurring costs — years of formation, fees, deference to lineage-based adjudication, and a standing discount on non-lineage competence — but imposes no systematic toll on continued participation and actively legitimizes the drift that other readings criminalize. Suppression is low (0.24): vernacular scholarship coexists, textual study is permitted and even honored, and the arrangement's persistence rests on habit and mutual benefit rather than on barring exits; what suppression exists is reputational (the 'barbarism' mark) rather than structural. Theater is low (0.18): the practice does its stated work daily — composition, administration, worship — with only a late-growing share of ceremonial purity-policing outpacing function. Accessibility_collapse is 0.35: alternatives demonstrably persist (self-instruction from grammars and edited texts, hybrid criteria, wholesale vernacular exit), so understanding the arrangement does not close the option set. Resistance is 0.28: philological reformers press hard from the late interval onward and under-formed clergy grumble, but mass resistance never materializes because most seats are net beneficiaries. The temporal series run on one shared six-point grid (800–1500) with both tracked metrics authored at every point; base_extractiveness creeps upward as schooling formalizes and the certification premium tightens, and theater rises gently as corrective ceremony grows while the practice's social base begins to contract. No suppression_requirement series is authored: enforcement capacity was structurally light and stable across the interval, and that static picture is already carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the masters' position the standard is simply what correctness is — their competence self-certifies, and the criterion (formed usage) is invisible as a choice because it is the medium they think in. From the under-formed clergyman's position the same standard is an imposed performance in a tongue he barely commands, enforced by correction from above. From the textual reformer's position it is institutional prejudice: evidence-bearing work discounted for lacking pedigree. The engine derives these divergent per-seat classifications from role, power, and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the masters and formed users near the subsidized end: the arrangement's product — certified correctness and a working medium — flows to them, and their exits are poor (identity_locked for the masters, whose authority dissolves outside the chain; constrained for formed users whose careers are built in the medium). Aspiring students sit intermediate: heavy front-loaded payment, deferred benefit, constrained exit once career paths commit. The two declared victim groups sit near the target end: under-formed clergy are trapped performers bearing the arrangement's daily costs with minimum voice, and textual reformers bear a standing discount they can escape only by leaving the jurisdiction entirely — mobile exit dampens their effective extraction somewhat below the trapped clergy's. Continental scope applies the engine's mild verification-difficulty amplification uniformly. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats correctly, and the derivation chain needs no correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a supralocal learned medium for a fragmented Europe — remained live across the entire interval, corroborated by paying users outside the beneficiary set, so no obsolescence flag is warranted and mandatrophy_resolved is false. The classification discipline cuts both ways here: against the snare mislabel, the victim set is thin and marginal, suppression is low, drift is explicitly legitimized, and real exits exist — this is not coercion wearing a coordination mask. Against the pure-rope mislabel, the honest 0.42 extractiveness records that the lineage premium and formation toll are real transfers, not zero-cost coordination, and the identity-coordination floor does not absorb them. The gentle upward creep in both tracked series is monitored rather than alarming: it reflects tightening certification value, not function atrophy — theater remains low and the founding problem live at interval end, though the omega on transmission fidelity marks where decay would first show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_fidelity_question,
    'Does unbroken practical transmission actually preserve authentic development, or does it accumulate undetected corruption relative to the Classical norm?',
    'Comparative philological reconstruction: align transmitted medieval forms with independently reconstructed Classical phonology, morphology, and syntax century by century, separating benign innovation from cumulative error attributable to the transmission chain itself.',
    'If divergence is mostly legitimate development, the reading''s legitimacy premise holds and the coordination-centered classification stands; if corruption dominates, the practice''s coordinating justification decays while its forms persist, effective extraction rises for every seat, and the arrangement drifts toward the degraded, inertially maintained end of the spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_fidelity_question, empirical, 'Whether the living chain transmits development or accumulates corruption — the empirical premise under the continuity reading''s legitimacy claim.').

omega_variable(
    criterion_of_correctness_underdetermination,
    'This story instantiates one reading of the classical_latin_standard kernel — is ''correctness as unbroken transmitted practice'' the only defensible framing of the operative standard, or do the reconstruction and hybrid readings capture the same arrangement better?',
    'Examine which criterion practicing institutions actually applied when correctness disputes arose — did chanceries and faculties correct by usage precedent or by appeal to ancient texts? — and test whether any single framework can operationalize competing criteria without contradiction.',
    'Adopting the reconstruction reading inverts the beneficiary/victim structure: the practice community becomes the corruption source and the textual workers become the injured party, with extraction re-pointed accordingly. Adopting the hybrid reading dissolves the pure lineage premium, moves adjudication toward textual experts, and lowers measured extraction for every outsider seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criterion_of_correctness_underdetermination, conceptual, 'Kernel-level framing underdetermination among the continuity, reconstruction, and hybrid readings of the classical Latin standard.').

omega_variable(
    formation_cost_vs_lineage_rent,
    'Is the price outsiders pay for standing — years of formation, fees, deference to practice-lineage adjudication — the genuine cost of transmitting an embodied skill, or a rent charged for admission to a credentialed caste?',
    'Benchmark formation length and cost against measurable competence outcomes, and against self-taught learners working from grammars and edited texts: if comparable competence is reachable far cheaper off the chain, the premium is rent; if on-chain formation uniquely produces the skill, it is cost.',
    'A rent finding raises effective extraction for the aspirant and outsider seats and pushes the arrangement toward the hybrid coordination/extraction territory; a cost finding confirms the coordination reading with residual extraction near the identity-coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_cost_vs_lineage_rent, empirical, 'Whether the lineage premium is skill-transmission cost or admission rent — the boundary between the rope and hybrid readings of this arrangement''s economics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 800, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t800, classical_latin_standard__continuity_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(clas_tr_t800, observed).
narrative_ontology:measurement(clas_tr_t950, classical_latin_standard__continuity_reading, theater_ratio, 950, 0.11).
narrative_ontology:measurement_basis(clas_tr_t950, observed).
narrative_ontology:measurement(clas_tr_t1100, classical_latin_standard__continuity_reading, theater_ratio, 1100, 0.13).
narrative_ontology:measurement_basis(clas_tr_t1100, observed).
narrative_ontology:measurement(clas_tr_t1250, classical_latin_standard__continuity_reading, theater_ratio, 1250, 0.14).
narrative_ontology:measurement_basis(clas_tr_t1250, observed).
narrative_ontology:measurement(clas_tr_t1400, classical_latin_standard__continuity_reading, theater_ratio, 1400, 0.16).
narrative_ontology:measurement_basis(clas_tr_t1400, observed).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__continuity_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement_basis(clas_tr_t1500, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t800, classical_latin_standard__continuity_reading, base_extractiveness, 800, 0.3).
narrative_ontology:measurement_basis(clas_be_t800, observed).
narrative_ontology:measurement(clas_be_t950, classical_latin_standard__continuity_reading, base_extractiveness, 950, 0.33).
narrative_ontology:measurement_basis(clas_be_t950, observed).
narrative_ontology:measurement(clas_be_t1100, classical_latin_standard__continuity_reading, base_extractiveness, 1100, 0.36).
narrative_ontology:measurement_basis(clas_be_t1100, observed).
narrative_ontology:measurement(clas_be_t1250, classical_latin_standard__continuity_reading, base_extractiveness, 1250, 0.39).
narrative_ontology:measurement_basis(clas_be_t1250, observed).
narrative_ontology:measurement(clas_be_t1400, classical_latin_standard__continuity_reading, base_extractiveness, 1400, 0.41).
narrative_ontology:measurement_basis(clas_be_t1400, observed).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__continuity_reading, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement_basis(clas_be_t1500, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(classical_latin_standard__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'correct Latin' conflates three structurally distinct claims with materially different epsilon values, beneficiary sets, and failure modes. This story authors the continuity reading (correctness = unbroken living practice, drift legitimate; moderate extraction via formation toll and lineage premium; thin victim fringe). The reconstruction reading (correctness = Classical form recovered by discontinuous return to texts, medieval drift rejected) inverts the moral structure: the practice community becomes the corruption source rather than the guarantor. The hybrid reading (textual fidelity plus licensed post-Classical development) splits the difference and redistributes adjudication to textual experts. The upstream continuity claim historically influenced both siblings — its accumulated, tolerated drift is precisely what made the reconstruction critique possible and the hybrid compromise attractive — so this story's edges point at both. Each member carries its own stable epsilon; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
