% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Correct Latin as Continuous Living Practice (Continuity Reading)
 *   domain: historical linguistics/philology/intellectual history
 *
 * SUMMARY:
 *   A normative standard governing written Latin operated across Latin
 *   Christendom from the Carolingian correction of texts (t0 ~= 850 CE;
 *   interval units are years since t0, tn ~= 1450) down to the eve of full
 *   humanist ascendancy. Under the continuity reading instantiated here,
 *   correctness is constituted by unbroken living transmission: schools,
 *   monastic scriptoria, and chanceries hand the language on, medieval usage
 *   counts as legitimate evolution of Classical Latin, and adjustment happens
 *   inside the practice rather than by reconstruction from ancient
 *   manuscripts. The arrangement solves a real problem — keeping one
 *   supraregional written language alive across political fragmentation —
 *   while transferring years of apprentice labor, tuition, and mediation fees
 *   to the transmitting institutions and foreclosing Latinate careers from
 *   those excluded from schooling. CONSTRAINT FAMILY NOTE: this file is one
 *   reading of the kernel correct_latin. The sibling files instantiate
 *   different constraints with different epsilon over the same standing
 *   arrangement: correct_latin__discontinuity_reading (medieval usage as
 *   corrupt deviation; the entire medieval output becomes an error-burden
 *   loaded onto learners, epsilon markedly higher) and
 *   correct_latin__hybrid_reading (partial continuity with textual
 *   correction; intermediate). The stories are linked through
 *   network.affects_constraints; the differing epsilon values are
 *   reading-indexed assessments of one fixed referent, not contradictions.
 *   KEY AGENTS (by structural relationship): - cathedral_school_masters:
 *   agenda-setting seat (institutional/identity_locked) — administers the
 *   standard, collects tuition and correction fees - monastic_scriptoria:
 *   beneficiary seat (institutional/identity_locked) — collects custodial
 *   prestige and resources - university_faculties_of_arts: agenda-setting
 *   seat (institutional/constrained) — licenses teachers, collects advanced
 *   fees - curial_chancery_officials: beneficiary seat with heavy personal
 *   cost-bearing (powerful/constrained) - excluded_women_literates: primary
 *   target seat (powerless/trapped) — bears foreclosure of the careers -
 *   vernacular_composers: target seat (moderate/constrained) — bears status
 *   and patronage displacement - parish_priests_with_deficient_latin: target
 *   seat (powerless/trapped) — bears correction, shame, remedial fees -
 *   lay_petitioners: target seat (powerless/immediate) — bears mediation fees
 *   for unread documents - humanist_philologists: excluded critic seat
 *   (moderate/constrained) — presses the rival reading from outside -
 *   modern_historical_linguists: analytical observer — attests transmission
 *   patterns without a seat in the dispute
 *
 * KEY AGENTS:
 *   - cathedral_school_masters: agenda-setting seat (institutional/identity_locked) — teaches, licenses, and corrects; collects tuition and correction fees; honor and livelihood fused with the chain they transmit
 *   - monastic_scriptoria: beneficiary seat (institutional/identity_locked) — copies and preserves; collects custodial prestige and resources
 *   - university_faculties_of_arts: agenda-setting seat (institutional/constrained) — controls licentia docendi and curriculum; enforces upstream of the schoolmasters
 *   - curial_chancery_officials: beneficiary seat with heavy personal cost-bearing (powerful/constrained) — drafts for fees after decades of dictamen apprenticeship
 *   - excluded_women_literates: primary target seat (powerless/trapped) — barred from the schooling the careers require
 *   - vernacular_composers: target seat (moderate/constrained) — honor and patronage rank below Latinate production
 *   - parish_priests_with_deficient_latin: target seat (powerless/trapped) — ordination threshold, remedial fees, chronic correction
 *   - lay_petitioners: target seat (powerless/immediate) — pays for documents it cannot read
 *   - humanist_philologists: excluded critic seat (moderate/constrained) — argues the rival reading from outside the licensing machinery
 *   - modern_historical_linguists: analytical observer (analytical/civilizational) — attests continuity and reset points empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.6).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.5).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Correct Latin as Continuous Living Practice (Continuity Reading)").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical linguistics/philology/intellectual history").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '0cf219eb-3f4c-4682-aa26-692dc7d75a66').
narrative_ontology:cs_kernel_codification('0cf219eb-3f4c-4682-aa26-692dc7d75a66', formalized).
narrative_ontology:cs_authority_grounding('0cf219eb-3f4c-4682-aa26-692dc7d75a66', practice).
narrative_ontology:cs_interpretation_layer_present('0cf219eb-3f4c-4682-aa26-692dc7d75a66').
narrative_ontology:cs_reading_relation('0cf219eb-3f4c-4682-aa26-692dc7d75a66', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0cf219eb-3f4c-4682-aa26-692dc7d75a66', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('0cf219eb-3f4c-4682-aa26-692dc7d75a66', foundational, continuous_transmission_confers_legitimacy).
narrative_ontology:cs_axiom_status(continuous_transmission_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0cf219eb-3f4c-4682-aa26-692dc7d75a66', continuous_transmission_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('0cf219eb-3f4c-4682-aa26-692dc7d75a66', secondary, internal_adjustment_suffices_for_reform).
narrative_ontology:cs_axiom_status(internal_adjustment_suffices_for_reform, holdable).
narrative_ontology:cs_axiom_grounding('0cf219eb-3f4c-4682-aa26-692dc7d75a66', internal_adjustment_suffices_for_reform, conventional).
narrative_ontology:cs_reference_frame('0cf219eb-3f4c-4682-aa26-692dc7d75a66', living_transmission_continuum).
narrative_ontology:cs_drift_state('0cf219eb-3f4c-4682-aa26-692dc7d75a66', humanist_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0cf219eb-3f4c-4682-aa26-692dc7d75a66', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, cathedral_school_masters).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, monastic_scriptoria).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, university_faculties_of_arts).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, curial_chancery_officials).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, excluded_women_literates).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, vernacular_composers).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, parish_priests_with_deficient_latin).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, lay_petitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach grammar and composition in the cathedral schools, hear pupils' exercises, and decide which usages pass correction. Collect tuition and correction fees from pupils and their families. Their honor, livelihood, and place in the clerical order rest entirely on the chain of teaching they themselves were formed in; stepping outside it would cost them the standing that makes them masters at all.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, cathedral_school_masters, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, cathedral_school_masters, beneficiary).

% Copy, preserve, and produce the written stock of the language in monastery workshops. Gain custodial prestige, donations, and privileged access to offices as the houses that keep the inherited books alive. Bound by vows to communities whose identity is inseparable from the transmission they perform.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, monastic_scriptoria, beneficiary,
    institutional, generational, identity_locked, continental).

% Set the curriculum, examine candidates, and grant the licence to teach. Collect matriculation and examination fees from an enrollment stream that depends on the Latinate career track remaining the route to office. Differ from the schoolmasters in enforcing upstream: they license the teachers, not only the taught.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, university_faculties_of_arts, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, university_faculties_of_arts, beneficiary).

% Draft bulls, letters, and legal instruments for the papal court and the great chanceries, collecting salaries and drafting fees. Reached their posts only after long apprenticeship in dictamen and competition under peer judgment of style; promotion and preferment ride on standing in that judgment.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, curial_chancery_officials, beneficiary,
    powerful, biographical, constrained, continental).

% Barred from grammar schools and universities, literate women could not enter the careers the schooling fed. Convents offered a narrow exception; outside it, command of the learned tongue was foreclosed regardless of talent, and the livelihoods attached to it were closed accordingly.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, excluded_women_literates, payer,
    powerless, biographical, trapped, regional).

% Compose songs, romances, and chronicles in the spoken tongues of courts and towns. See honor, patronage, and permanence flow to Latinate production while their own work ranks as entertainment; switching to the learned language means abandoning the audiences that sustain them.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_composers, payer,
    moderate, biographical, constrained, national).

% Hold rural benefices after clearing an ordination examination many barely pass. Endure correction and ridicule from visiting superiors, pay for remedial schooling out of slender income, and depend on the same standard that measures them falling short.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, parish_priests_with_deficient_latin, payer,
    powerless, biographical, trapped, local).

% Need charters, contracts, wills, and court pleadings drawn up in the learned tongue by trained clerks. Pay fees for documents they cannot read and must trust or have read aloud to them; their transactions move through a medium they do not control.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, lay_petitioners, payer,
    powerless, immediate, trapped, local).

% From Petrarch's generation onward, argue that the inherited usage has slipped from the ancient models and press for return to recovered texts. Stand outside the schools' licensing machinery; their style draws mockery from the masters even as princely and curial patronage gives them growing leverage.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, humanist_philologists, excluded,
    moderate, biographical, constrained, continental).

% Study the transmission with the tools of philology and linguistics: stemmatics, orthographic series, usage corpora. Attest where usage descends continuously and where it was reset, without holding a seat in any of the disputing camps.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, cathedral_school_masters).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single evolving written Latin intelligible across regions and generations: charters, liturgy, law, and scholarship remain readable without each generation reconstructing the language from ancient texts; schools transmit competence cumulatively from master to pupil.
% TRANSFER_FUNCTION: Moves years of apprentice labor and tuition from pupils and their families to schoolmasters and university faculties; moves drafting and mediation fees from lay petitioners to trained clerks; moves honor, office, and benefice income to those certified inside the transmission chain.
% ABSENT_VOICES: Women barred from grammar schooling, vernacular poets ranked beneath Latinate production, poorly trained rural clergy, and lay petitioners unable to read what they paid to have drafted — none held seats in the schools, chapters, or faculties where correctness was decided. The humanist critics entered the conversation only at the interval's end, from outside the licensing machinery.
% DISAPPEARANCE_RATIONALE: If the standard and its transmission vanished overnight, cross-border charters, liturgy, university teaching, and diplomatic correspondence lose their common medium; the documentary apparatus of Latin Christendom reorganizes around the vernaculars and Greek centuries ahead of schedule, and the schools, scriptoria, and chanceries that lived on the chain dissolve with it.
% FOUNDING_PROBLEM: After the western empire's administrative collapse, political fragmentation shattered spoken unity: records, worship, and diplomacy needed one supraregional written language that could be taught, corrected, and kept alive without a state behind it.
% FOUNDING_PROBLEM_CORROBORATION: Municipal councils and merchant guilds — fee-paying but demand-dependent — attest the live need in their notarial contracts; royal and papal chancery correspondence attests it from the consuming side; the survival of tenth-century records only in the learned tongue attests that no vernacular alternative yet carried the load. Corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.60 at interval end: the chain performs a real service (documents work across borders and centuries) while rents accumulate on top of it — tuition above instructional cost, mediation fees for a medium the payers cannot audit, and a monopoly on honor and office. Suppression is authored at 0.50 as a RAW structural property (career closure for the unlicensed, correction discipline inside the schools); it is deliberately unscaled — the engine scales only extractiveness, by directionality and scope. Theater ratio ends at 0.35: transmission remains mostly functional, but by the late interval a growing share of activity is defensive performance — style defenses of medieval usage mounted against humanist mockery — which is symptom, not test. Accessibility collapse sits at 0.55: opting out was possible through the vernaculars but costly inside the elite career track, so alternatives narrowed without vanishing. Resistance at 0.55 reflects goliardic irreverence, vernacular self-assertion, and the culminating humanist attack. TEMPORAL GRID: all three tracked metrics share one seven-point grid (0..600, step 100) — no per-metric grids. The suppression_requirement series is authored because enforcement capacity visibly changed: build-up through licentia docendi and benefice discipline peaks near t=300, then decays as humanist patronage opens exits. Extractiveness accumulates monotonically (T17-relevant rent-layering as the institutions consolidated); theater rises in step with the defensive turn. The trajectories are monotonic trends, not cycles — no intermittent-reinforcement mechanism is claimed. IDENTITY LOCK: the masters' and scriptoria's fusion is professional-institutional — their honor, salvation-historical self-understanding, and daily practice are the transmission; if that frame broke (as under humanist victory), their seat flips from administrator to target of the successor regime. COALITION NOTE: the scattered powerless targets (women, rural clergy, petitioners) lacked channels to combine; princely vernacular courts eventually supplied one, feeding the end-interval resistance rise. SAME-LEVEL LATERAL: schoolmasters and university faculties hold equal nominal standing but differentiated enforcement positions — faculties license the masters' output upstream — so identical power atoms resolve to different exits and exposures. RECEIPT SURFACE: the largest measurable flow, tuition and correction fees, lands on the cathedral_school_masters seat, so gain_flow names it; faculties and chanceries collect secondary shares at later stages, which is why the primary seat is named rather than a universal negative asserted. FIXING COST: removal was prohibitive for any contemporary actor — no alternative supraregional written medium stood ready until the vernaculars matured centuries later. BOLTZMANN TYPE: information_standard is declared because the dominant function is keeping a written medium interoperable across regions and generations; the class-membership effects are real but derivative. Declaring identity_coordination instead would grant extra coupling leeway on the strength of precisely the identity framing that partly serves as cover, so the conservative type is chosen.
 *
 * PERSPECTIVAL GAP:
 *   From the masters' and scriptoria's seats the standard is simply the language living — continuity is experienced as inheritance, not imposition, and the fees feel like the price of a hard-won art. From the excluded women's and the petitioners' seats the same apparatus is a wall priced in fees and foreclosed livelihoods. Parish clergy experience it as chronic shame measured against a bar they barely clear. The engine computes these per-seat classifications from power, exit, and declared position; the divergence between the near-beneficiary agenda-setter seats and the trapped payer seats is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the four transmitting seats near the subsidized end; the victim declarations place the four paying seats near the target end, amplified by trapped and identity-locked exits; the excluded humanists and the analytical observer sit outside the derivation. ONE OVERRIDE: curial_chancery_officials carry power_atom 'powerful', and the structural derivation from their beneficiary role would place them near the beneficiary end (~0.1); but they personally bear decades of apprenticeship and competitive examination costs, putting their true position nearer symmetric — overridden to d=0.30. No other override is used: everywhere else the derivation from declared relationships plus exit options matches the structural facts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a supraregional written language without a state behind it — stayed live across the whole interval, so mandatrophy_resolved is false and no sunset applies. The classification guards against both mislabels: the continuity reading's own self-description ('the language simply lived') would flatten the arrangement to pure coordination, erasing the tuition, fee, and foreclosure transfers; the humanist polemic ('barbarous corruption') would flatten it to pure extraction, erasing the real service the chain performed for eleven centuries. The structural data — a genuine coordination function plus an enforced, asymmetric transfer with named payers — supports the hybrid classification claimed here as tangled_rope, and the engine computes each seat's type independently from that data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel correct_latin; which reading a community adopts determines who counts as correct and who bears correction costs — how does the classification shift under the sibling readings?',
    'Not resolvable by data alone: track which criterion (transmitted practice, ancient text, mixed) successive institutions actually enforce — Carolingian schools, twelfth-century faculties, fifteenth-century curia — and classify each regime under the reading it embodies.',
    'Under correct_latin__discontinuity_reading the victim set expands to every medieval practitioner (forced unlearning) and epsilon rises sharply; under correct_latin__hybrid_reading costs concentrate on disputed forms only. The present story''s values hold only for the continuity seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the correct-Latin kernel; sibling readings change the victim set and epsilon.').

omega_variable(
    constructed_continuity_ambiguity,
    'Was the transmission actually continuous, or did Carolingian standardization and later curricular consolidation manufacture the appearance of a single unbroken chain?',
    'Manuscript stemma and orthographic-series analysis across the interval: measure whether regional usages descend from one another or were repeatedly reset to imported exemplars.',
    'If continuity is partly retrospective construction, the reading''s legitimacy claim weakens, the arrangement recharacterizes as an imposed standard, and effective extraction on the peripheral seats rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_continuity_ambiguity, empirical, 'Whether the continuity the reading presupposes is a lived fact or an institutional construction.').

omega_variable(
    apprenticeship_cost_vs_rent,
    'How much of the measured transfer is the inherent cost of acquiring a demanding literate register, and how much is institutional rent collected above that cost?',
    'Compare instructional costs, fee schedules, and the lifetime premium of Latinate-certified careers against equivalent non-Latinate skilled trades; surviving fee ledgers and benefice accounts supply the series.',
    'A large rent component supports the asymmetric-transfer reading of the arrangement; a small one would push the arrangement toward ordinary coordination cost and soften the payer-seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprenticeship_cost_vs_rent, empirical, 'Decomposing the transfer into acquisition cost versus institutional rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t100, correct_latin__continuity_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement(corr_tr_t200, correct_latin__continuity_reading, theater_ratio, 200, 0.19).
narrative_ontology:measurement(corr_tr_t300, correct_latin__continuity_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(corr_tr_t400, correct_latin__continuity_reading, theater_ratio, 400, 0.26).
narrative_ontology:measurement(corr_tr_t500, correct_latin__continuity_reading, theater_ratio, 500, 0.31).
narrative_ontology:measurement(corr_tr_t600, correct_latin__continuity_reading, theater_ratio, 600, 0.35).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(corr_be_t100, correct_latin__continuity_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement(corr_be_t200, correct_latin__continuity_reading, base_extractiveness, 200, 0.49).
narrative_ontology:measurement(corr_be_t300, correct_latin__continuity_reading, base_extractiveness, 300, 0.53).
narrative_ontology:measurement(corr_be_t400, correct_latin__continuity_reading, base_extractiveness, 400, 0.56).
narrative_ontology:measurement(corr_be_t500, correct_latin__continuity_reading, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(corr_be_t600, correct_latin__continuity_reading, base_extractiveness, 600, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__continuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(corr_su_t100, correct_latin__continuity_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(corr_su_t200, correct_latin__continuity_reading, suppression_requirement, 200, 0.57).
narrative_ontology:measurement(corr_su_t300, correct_latin__continuity_reading, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(corr_su_t400, correct_latin__continuity_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(corr_su_t500, correct_latin__continuity_reading, suppression_requirement, 500, 0.54).
narrative_ontology:measurement(corr_su_t600, correct_latin__continuity_reading, suppression_requirement, 600, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' covers three structurally distinct claims (epsilon-invariance decomposition): legitimacy by transmitted practice (this file), legitimacy by ancient text (correct_latin__discontinuity_reading), and a mixed criterion (correct_latin__hybrid_reading). Each carries its own epsilon, beneficiaries, and victims; the files are linked through affects_constraints. The influence pattern runs diachronically: the continuity arrangement's enforcement history is the evidence base the discontinuity reading cites when it attacks the chain, so the upstream story feeds the downstream critique.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__continuity_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
