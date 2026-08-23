% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Correctness Settlement (Hybrid Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   Between the fifteenth-century humanist recovery of ancient texts and the
 *   vernacular ascendancy around 1600, learned Europe operated a bifurcated
 *   correctness settlement for Latin: classical norms reconstructed from
 *   Cicero and the ancient canon governed literary and rhetorical prose,
 *   while technical and practical registers (law, medicine, administration,
 *   scholastic theology) retained their working medieval forms under a
 *   legitimacy clause. This story instantiates the HYBRID READING of the
 *   contested latin_correctness kernel: the claim that correctness is
 *   legitimately domain-relative. The epsilon referent is the standing
 *   bifurcated arrangement itself, assessed by this reading's own lights —
 *   not the rupture reading's universal classical enforcement or the
 *   continuity reading's wholesale legitimization, which are separate
 *   constraints (sibling files linked via network.affects_constraints). The
 *   hybrid reading endorses the bifurcation's coordination work while its own
 *   authored metrics record what the settlement costs: a status hierarchy
 *   ranking literary above technical prose, a discretionary
 *   literary/technical boundary policed by the party that occupies its
 *   prestigious side, and a partial victim set of technical writers pressed
 *   toward classical standards their subject matter cannot meet. Claim and
 *   metrics are independent authored facts: claimed_type is tangled_rope from
 *   the structure (genuine coordination on both sides of the bifurcation plus
 *   asymmetric standing costs held in place by active enforcement); the
 *   metric values are authored from the descriptive record without tuning
 *   toward that claim.
 *
 * KEY AGENTS:
 *   - humanist_literati: agenda-setter and primary beneficiary seat (institutional/arbitrage) — reconstructs classical usage, staffs schools and presses, adjudicates the literary/technical boundary, and collects the settlement's cultural authority
 *   - technical_prose_authors: primary payer seat (moderate/constrained) — physicians, natural philosophers, jurists, and administrators whose working Latin is legitimized but ranked below literary prose
 *   - scholastic_theologians: dual-positioned seat (organized/identity_locked) — sheltered by the technical-domain clause yet subordinated by the literary hierarchy; exit would repudiate their method
 *   - classical_grammar_schoolmasters: beneficiary seat (organized/constrained) — livelihood bound to the literary domain's classical requirement
 *   - elite_patrons: beneficiary seat (powerful/arbitrage) — consume classical polish as a marker of cultivated rank
 *   - printing_house_editors: enforcement seat (institutional/mobile) — harden the boundary into typographic practice without originating the norms
 *   - vernacular_authors: excluded seat (moderate/mobile) — stand outside the settlement and contest its Latin premise
 *   - historians_of_latin: analytical observer — reconstructs the settlement's full structure from curricula, printers' records, prefaces, and correspondence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.58).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.55).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Correctness Settlement (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'f3b32efd-6404-47bd-a236-01a1b728f29e').
narrative_ontology:cs_kernel_codification('f3b32efd-6404-47bd-a236-01a1b728f29e', distributed).
narrative_ontology:cs_authority_grounding('f3b32efd-6404-47bd-a236-01a1b728f29e', expertise).
narrative_ontology:cs_interpretation_layer_present('f3b32efd-6404-47bd-a236-01a1b728f29e').
narrative_ontology:cs_reading_relation('f3b32efd-6404-47bd-a236-01a1b728f29e', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3b32efd-6404-47bd-a236-01a1b728f29e', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('f3b32efd-6404-47bd-a236-01a1b728f29e', foundational, domain_relative_linguistic_legitimacy).
narrative_ontology:cs_axiom_status(domain_relative_linguistic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f3b32efd-6404-47bd-a236-01a1b728f29e', domain_relative_linguistic_legitimacy, instrumental).
narrative_ontology:cs_axiom('f3b32efd-6404-47bd-a236-01a1b728f29e', foundational, classical_normative_authority_in_literary_register).
narrative_ontology:cs_axiom_status(classical_normative_authority_in_literary_register, holdable).
narrative_ontology:cs_axiom_grounding('f3b32efd-6404-47bd-a236-01a1b728f29e', classical_normative_authority_in_literary_register, conventional).
narrative_ontology:cs_reference_frame('f3b32efd-6404-47bd-a236-01a1b728f29e', diglossic_classical_technical_settlement).
narrative_ontology:cs_drift_state('f3b32efd-6404-47bd-a236-01a1b728f29e', late_sixteenth_century_purist_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3b32efd-6404-47bd-a236-01a1b728f29e', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_literati).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_grammar_schoolmasters).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, elite_patrons).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_prose_authors).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scholastic_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, scholastic_theologians).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_relative_correctness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reconstruct classical usage from ancient texts, staff grammar schools and university arts faculties, edit printed books, and adjudicate which registers count as literary. They collect status, patronage, and educational authority from the classical standard they administer; their dual position — setting the literary/technical boundary while occupying its prestigious side — is the seat the settlement's cultural authority accrues to. Their philological skills transfer across courts, universities, and printing trades, so exit is cheap for them.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_literati, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, humanist_literati, beneficiary).

% Teach the classical curriculum that the settlement requires for the literary track. Their livelihood and professional standing depend on the literary domain's classical requirement remaining in force; they collect fees and enrollment from families seeking classical training. Leaving the trade would forfeit specialized standing built over a career.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_grammar_schoolmasters, beneficiary,
    organized, generational, constrained, continental).

% Courts, prelates, and magistrates who consume elegant Latin as a marker of cultivated rank. The literary domain's classical requirement makes their patronage scarce and status-bearing; they fund humanists and demand classical polish in dedications and orations. Patronage follows prestige wherever it sits, so their position is secure under any correctness regime that produces distinction.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, elite_patrons, beneficiary,
    powerful, generational, arbitrage, continental).

% Physicians, natural philosophers, jurists, and administrators whose working Latin carries the medieval terminology and syntax their subjects require. The settlement's technical-domain clause legitimizes their registers, but their works are received as second-class, their prefaces apologize for inelegant style, and ambitious texts are pressed toward classical polish their content cannot bear. Latin remains the only pan-European learned medium through most of the interval, and moving to vernacular forfeits that readership while it matures.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_prose_authors, payer,
    moderate, biographical, constrained, continental).

% University theologians whose disputed-question method and technical vocabulary are sheltered by the settlement's technical-domain clause, yet whose genre is ranked below literary prose. Their public-facing texts — prefaces, dedications, published disputations — are pressed toward classical norms their method cannot accommodate. Their intellectual identity is fused with the scholastic method and its Latin; abandoning that Latin would repudiate their training, their tradition, and their institution's self-understanding.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scholastic_theologians, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, scholastic_theologians, beneficiary).

% Printers and correctors who decide which texts receive classical normalization: literary works are emended toward ancient usage while practical formularies are set in working Latin. Their house rules harden the settlement's boundary into typographic practice. They enforce norms they did not originate, profit from the classical editions trade the settlement created, and can shift lines toward what sells.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, printing_house_editors, agenda_setter,
    institutional, biographical, mobile, continental).

% Writers working in Italian, French, English, and German who stand outside the Latin settlement entirely. The bifurcation debate assumes Latin's centrality; these authors would contest the premise that learned writing must be Latin at all rather than any term of the settlement. Their growing vernacular readerships are the exit route the settlement's constrained seats cannot yet take.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_authors, excluded,
    moderate, biographical, mobile, national).

% Modern philologists and historians of the Latin language who reconstruct the settlement's operation from curricula, printers' records, prefaces, and correspondence. They see the whole structure — the coordination work on both sides of the bifurcation, the ranking of registers, the boundary-policing — from outside any seat's stake.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, historians_of_latin, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, humanist_literati).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single written Latin adequate for pan-European learned communication while matching normative strictness to function: the literary and rhetorical register shares one classical standard (a common canon, mutual readability, stylistic excellence across Europe), while technical and practical registers retain working medieval forms (the precision vocabulary and established syntax that law, medicine, administration, and scholastic theology operate in). The bifurcation also coordinates education: it defines what grammar schools must teach and what practical training may skip.
% TRANSFER_FUNCTION: Moves cultural authority and standing from technical and scholastic prose to classical literary prose; moves educational time, fees, and patronage toward classical literary training; and moves editorial care toward texts judged literary — technical works receive less correction, fewer prestigious printings, and lower standing in the respublica literaria.
% ABSENT_VOICES: Vernacular authors and advocates of vernacular learning are outside the settlement entirely; they would contest the premise that learned writing must be Latin rather than the settlement's internal terms. The technical practitioners sit as payers whose objections surface mainly as preface apologies rather than counter-proposals. Women, excluded from Latin schooling altogether, are absent from every seat: each reading of the kernel assigns them a gate none of the readings debates.
% DISAPPEARANCE_RATIONALE: If the bifurcated settlement vanished overnight, the kernel contest would resolve to a sibling: rupture's universal classical enforcement (technical registers burdened or abandoned, scholastic and administrative Latin condemned as corruption) or continuity's wholesale legitimization (the shared literary classical standard dissolves and the humanist cultural project loses its object). Education, printing lines, and patronage expectations would reorganize around whichever successor held, and over the longer run the settlement's disappearance would accelerate the vernacular takeover of both registers.
% FOUNDING_PROBLEM: The fifteenth-century humanist recovery of ancient texts created a correctness crisis: medieval Latin had drifted far from classical usage, and the reconstructed classical norm could not be imposed everywhere without breaking the working registers that law, medicine, administration, and scholastic theology depended on. The hybrid settlement was built to restore a classical literary culture while leaving the functional registers free to operate.
% FOUNDING_PROBLEM_CORROBORATION: Scholastic theologians and university faculties — payers, not beneficiaries — attest the technical-register problem is live, defending their method's Latin in statutes and disputations; the printing trade corroborates it commercially by maintaining separate classical and practical lines across the whole interval. No party claims the founding problem is dead: rupture-leaning purists say the classical restoration is unfinished, continuity-leaning traditionalists say the registers remain threatened — both attest the underlying allocation problem is still open, from outside the beneficiary set.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is moderate: both sides of the bifurcation do real coordination work (a shared classical standard for literary prose; functional legitimacy for technical registers), but the same structure ranks registers, and the ranking's costs fall on seats that do not share its governance. The costs concentrate at two points: the discretionary literary/technical boundary, where domain-assignment decisions allocate prestige case by case, and the literary hierarchy itself, which ranks technical prose below literary prose regardless of competence. Suppression 0.55 is structural first (grammar-school curricula, print correction, patronage expectations, university statutes) with a growing internalized component (preface apologies for barbarous Latin); alternatives persist — scholastic practice continues, vernacular exit is opening — so suppression stays below the enforced-monopoly range. Theater_ratio 0.38 and rising: Ciceronian purism becomes increasingly performative across the interval (slavish imitation, purist-on-purist vocabulary policing, and the satire it attracted), while the literary classical standard retains genuine function. Accessibility_collapse 0.42: the settlement's own technical-domain clause keeps alternatives partly open, and vernacular publication erodes the Latin monopoly from outside. Resistance 0.55: scholastic defense of method, the Ciceronian controversy among humanists themselves, technical authors' quiet persistence in working Latin, and vernacular advocacy. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the apparatus builds from loose learned preference (1400) through institutionalized curriculum and print standardization to a purist peak (1560), then eases slightly by 1600 as vernacular exit opens and the marginal enforcement burden shifts. All three series share one six-point grid so drift dating is not distorted by substitution.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the humanist seat the settlement is a restoration achieved: classical Latin saved from both dissolution (the continuity alternative) and petrification into an unlivable universal code (the rupture alternative), with the discipline it imposes a price worth paying. From the technical author's seat the same structure is a ceiling: legitimacy granted and standing withheld, with the boundary drawn by the party that sits on its rewarding side. From the scholastic seat it is shelter and subordination at once — the technical-domain clause protects a method whose public standing the literary hierarchy strips. The printing seat experiences the settlement as a trade practice, enforceable and adjustable. Inter-institutionally, universities (scholastic method), the printing trade (house rules), courts (patronage), and grammar schools (curriculum) hold different stakes at nominally similar institutional power; same-level learned actors differ by exit — the scholastic's identity lock against the technical author's constrained-but-real vernacular option. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (humanist_literati, classical_grammar_schoolmasters, elite_patrons) derive low directionality: the settlement subsidizes them with standing, enrollment, and patronage relevance, and their exits run from constrained to arbitrage-grade. technical_prose_authors derive high directionality: they bear the transfer (devaluation, unattainable polish pressure) with constrained exit, since Latin remains the only pan-European learned medium through most of the interval. scholastic_theologians sit between: the victim listing pushes them toward the target end, their technical-domain benefit damps it, and identity lock (their method IS their Latin) amplifies target-side exposure because they cannot cheaply become the classical authors the hierarchy rewards. printing_house_editors are near-symmetric enforcement intermediaries: they apply norms they did not originate and can shift lines toward what sells. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and spatial scope in the engine's computation. No directionality overrides are declared: the beneficiary/victim declarations plus exit atoms already separate the seats, and the one dual-positioned agent is handled by secondary_role — an override keyed to the organized power atom would also capture classical_grammar_schoolmasters, who are clean beneficiaries. Receipt: the settlement's gains demonstrably accrue to the humanist_literati seat, which administers the boundary and occupies its rewarding side; schoolmasters and patrons collect derivative benefits, so gain_flow names the humanist seat. fixing_cost is prohibitive: the seats that could restructure the arrangement (the humanist elite and its patrons) would forfeit the cultural capital the settlement constitutes for them, and the arrangement is woven through curricula, print practice, and patronage expectation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the settlement as tangled_rope rather than snare prevents reading the status hierarchy as pure taking behind a coordination cover story: the bifurcation solves a real problem neither sibling solves — rupture's universal classical code would have broken the working registers law, medicine, and theology operate in, and continuity's wholesale legitimization would have dissolved the shared literary standard the respublica literaria ran on. Classifying it as tangled_rope rather than rope prevents the opposite error: the same boundary that coordinates also ranks, the ranking is policed by its beneficiaries, and a partial victim set bears costs the beneficiary seats do not share. Mandatrophy: the founding problem — reconcile the classical restoration with functional registers — stays live across the whole interval, so mandatrophy_resolved is not declared. The settlement is not maintained after its function died; it is eventually dissolved from outside, by vernacularization removing the substrate every reading of the kernel presupposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the latin_correctness kernel; how much of its measured structure is reading-indexed rather than a property of learned Latin practice as such?',
    'Compile the sibling readings (latin_correctness__continuity_reading, latin_correctness__rupture_reading) and compare beneficiary/victim structure and extraction at shared time points; the delta across readings isolates the component the reading choice carries.',
    'Under the continuity reading the victim set largely dissolves (no unattainable standard is imposed anywhere) and measured extraction falls toward the coordination floor; under the rupture reading enforcement universalizes, the victim set expands to every medieval-form register, and extraction rises sharply. The hybrid reading''s moderate profile holds only while the bifurcation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexed component of the bifurcated settlement''s structure within the contested latin_correctness kernel.').

omega_variable(
    boundary_demarcation_ambiguity,
    'What fixes the boundary between literary/rhetorical and technical/practical domains, and who adjudicates marginal cases (a dedicatory epistle, a published disputation, a mathematical treatise with rhetorical pretensions)?',
    'Reception and production records: which texts were corrected toward classical norms by editors and which were left in working Latin, university statutes on style, printers'' house rules, patron correspondence requesting polish.',
    'If the boundary is discretionary case-by-case adjudication by the humanist elite, the bifurcation operates as a discretion regime and the measured extraction understates it (the boundary itself is the prize); if genre conventions fix it, extraction is bounded and the settlement is closer to a working diglossia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_demarcation_ambiguity, empirical, 'Whether the literary/technical boundary is convention-fixed or discretion-policed.').

omega_variable(
    unattainable_standard_ambiguity,
    'Are the classical standards pressed on technical writers actually unattainable for their subject matter, or attainable at a training cost the victim claim overstates?',
    'Compare technical authors'' Latin against classical benchmarks and code the criticism they attracted: features fixable by effort (orthography, periodic style) versus structural features their content requires (scholastic logical terminology, technical neologisms with no Ciceronian equivalent, abbreviatory syntax in practical documents).',
    'If attainable, the pressure is a regressive training tax and the victim set shrinks toward the genuinely structural cases; if structurally unattainable, the pressure withholds rank for features the register cannot shed and the victim claim stands at full strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unattainable_standard_ambiguity, empirical, 'Whether the classical standard is structurally unattainable for technical registers or merely costly.').

omega_variable(
    latin_monopoly_dependence,
    'Is the technical writer''s constrained exit a property of this settlement or of Latin''s monopoly on learned communication generally, and does vernacular viability rather than any change in the settlement itself dissolve the constraint?',
    'Track vernacular technical publication across and after the interval (Italian and French mathematical and medical treatises, later Latin-external scientific publishing): if technical authors exit as vernacular markets mature while the settlement''s rules are unchanged, the exit constraint was monopoly-carried.',
    'If monopoly-carried, the settlement''s hold on its payer seats is contingent on a substrate it does not itself enforce, and the constraint''s dissolution dates to vernacular takeoff rather than to any reform of the settlement''s rules.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latin_monopoly_dependence, empirical, 'Whether the settlement''s hold on technical writers rides on Latin''s communication monopoly.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression technical authors experience structural (curricula, print correction, patronage expectations, university statutes) or internalized (preface apologies for barbarous Latin, self-devaluation of technical registers, reluctance to claim literary standing)?',
    'Post-exit trajectory: technical authors who move to vernacular publishing or find classical-tolerant patrons — does the deference persist in their self-presentation, or does it drop with the enforcement environment?',
    'If substantially internalized, effective suppression exceeds the structural measure and the status hierarchy would outlive enforcement relaxation, persisting in self-presentation even where the rules lapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized share of the settlement''s suppression of technical authors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 1400, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1400, latin_correctness__hybrid_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement_basis(lati_tr_t1400, observed).
narrative_ontology:measurement(lati_tr_t1440, latin_correctness__hybrid_reading, theater_ratio, 1440, 0.16).
narrative_ontology:measurement_basis(lati_tr_t1440, observed).
narrative_ontology:measurement(lati_tr_t1480, latin_correctness__hybrid_reading, theater_ratio, 1480, 0.22).
narrative_ontology:measurement_basis(lati_tr_t1480, observed).
narrative_ontology:measurement(lati_tr_t1520, latin_correctness__hybrid_reading, theater_ratio, 1520, 0.29).
narrative_ontology:measurement_basis(lati_tr_t1520, observed).
narrative_ontology:measurement(lati_tr_t1560, latin_correctness__hybrid_reading, theater_ratio, 1560, 0.35).
narrative_ontology:measurement_basis(lati_tr_t1560, observed).
narrative_ontology:measurement(lati_tr_t1600, latin_correctness__hybrid_reading, theater_ratio, 1600, 0.38).
narrative_ontology:measurement_basis(lati_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t1400, latin_correctness__hybrid_reading, base_extractiveness, 1400, 0.34).
narrative_ontology:measurement_basis(lati_be_t1400, observed).
narrative_ontology:measurement(lati_be_t1440, latin_correctness__hybrid_reading, base_extractiveness, 1440, 0.4).
narrative_ontology:measurement_basis(lati_be_t1440, observed).
narrative_ontology:measurement(lati_be_t1480, latin_correctness__hybrid_reading, base_extractiveness, 1480, 0.46).
narrative_ontology:measurement_basis(lati_be_t1480, observed).
narrative_ontology:measurement(lati_be_t1520, latin_correctness__hybrid_reading, base_extractiveness, 1520, 0.52).
narrative_ontology:measurement_basis(lati_be_t1520, observed).
narrative_ontology:measurement(lati_be_t1560, latin_correctness__hybrid_reading, base_extractiveness, 1560, 0.56).
narrative_ontology:measurement_basis(lati_be_t1560, observed).
narrative_ontology:measurement(lati_be_t1600, latin_correctness__hybrid_reading, base_extractiveness, 1600, 0.58).
narrative_ontology:measurement_basis(lati_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1400, latin_correctness__hybrid_reading, suppression_requirement, 1400, 0.28).
narrative_ontology:measurement_basis(lati_su_t1400, observed).
narrative_ontology:measurement(lati_su_t1440, latin_correctness__hybrid_reading, suppression_requirement, 1440, 0.36).
narrative_ontology:measurement_basis(lati_su_t1440, observed).
narrative_ontology:measurement(lati_su_t1480, latin_correctness__hybrid_reading, suppression_requirement, 1480, 0.44).
narrative_ontology:measurement_basis(lati_su_t1480, observed).
narrative_ontology:measurement(lati_su_t1520, latin_correctness__hybrid_reading, suppression_requirement, 1520, 0.53).
narrative_ontology:measurement_basis(lati_su_t1520, observed).
narrative_ontology:measurement(lati_su_t1560, latin_correctness__hybrid_reading, suppression_requirement, 1560, 0.58).
narrative_ontology:measurement_basis(lati_su_t1560, observed).
narrative_ontology:measurement(lati_su_t1600, latin_correctness__hybrid_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement_basis(lati_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The natural-language label Latin correctness decomposes into three structurally distinct constraints (one per reading of the kernel) with distinct epsilon values: continuity_reading (low — legitimizes existing practice, minimal enforcement), hybrid_reading (moderate — bifurcated legitimacy with a status hierarchy; this file), and rupture_reading (high — universal classical enforcement, medieval usage condemned). They form one constraint family, linked via affects_constraints. The hybrid reading influences the rupture reading structurally: the settlement's grammar-school curriculum and philological publishing build the classical apparatus (trained classicists, edited texts, print standardization) that rupture's universalization program later leverages, without resolving the kernel dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
