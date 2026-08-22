% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Feudal-Obsolescence Reading of Magna Carta's Authority
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   The standing arrangement under contest: Magna Carta occupies a place of
 *   extraordinary ceremonial veneration in the constitutional culture of the
 *   United Kingdom and the common-law world while exerting, by this reading's
 *   account, no binding authority over modern sovereignty structures. Of the
 *   1297 charter's provisions, all but a handful stand repealed; the
 *   survivors are cited confirmatorily if at all; restraint claims grounded
 *   in the text fail as historical rhetoric. This story instantiates the
 *   feudal-obsolescence reading of the magna_carta_constraint_authority
 *   kernel: the charter was a baronial compact addressing thirteenth-century
 *   feudal grievances — scutage, wardship, alien mercenaries, forest access —
 *   whose normative force expired with the world that produced it. The
 *   reading's own assessment of the standing arrangement is nonetheless
 *   severe: the arrangement strips the charter's obligations while continuing
 *   to draw on its prestige, transferring the difference to executive
 *   discretion. Per the epsilon-invariance principle, the sibling readings
 *   (living constitutionalism; parliamentary sovereignty) are separate
 *   constraints with their own files; the contest is routed to omega
 *   variables, not averaged into this story's numbers. The claimed type and
 *   the metrics are independent authored facts: the type is stated from the
 *   structural reading of who receives what; the metrics describe the
 *   arrangement's observed operation. KEY AGENTS (by structural
 *   relationship): - modern_executive_governments: Primary beneficiary
 *   (institutional/arbitrage) — collects the discretion premium and harvested
 *   legitimation - uk_parliament: Agenda setter (institutional/arbitrage) —
 *   holds the revision pen, confirms the arrangement by inaction -
 *   senior_judiciary: Enforcement administrator (institutional/constrained) —
 *   disposes of charter claims, administers non-justiciability -
 *   popular_constitutionalists: Primary victim (organized/identity_locked) —
 *   organizing instrument deflated - charter_claim_litigants: Direct victim
 *   (powerless/trapped) — bears failed-claim costs one case at a time -
 *   higher_law_jurists: Secondary victim (moderate/constrained) — loses the
 *   fundamental-law warrant - charter_heritage_public: Excluded voice
 *   (powerless/constrained) — taught veneration, no seat in settlement -
 *   heritage_commemoration_industry: Incidental beneficiary
 *   (organized/mobile) — monetizes veneration without obligation -
 *   constitutional_historians: Analytical observer (analytical/analytical) —
 *   sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Feudal-Obsolescence Reading of Magna Carta's Authority").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'a438a642-5116-402e-a4b5-b6df95509233').
narrative_ontology:cs_kernel_codification('a438a642-5116-402e-a4b5-b6df95509233', fixed_text).
narrative_ontology:cs_authority_grounding('a438a642-5116-402e-a4b5-b6df95509233', practice).
narrative_ontology:cs_interpretation_layer_present('a438a642-5116-402e-a4b5-b6df95509233').
narrative_ontology:cs_reading_relation('a438a642-5116-402e-a4b5-b6df95509233', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('a438a642-5116-402e-a4b5-b6df95509233', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('a438a642-5116-402e-a4b5-b6df95509233', foundational, charter_normative_force_expired_with_feudal_context).
narrative_ontology:cs_axiom_status(charter_normative_force_expired_with_feudal_context, holdable).
narrative_ontology:cs_axiom_grounding('a438a642-5116-402e-a4b5-b6df95509233', charter_normative_force_expired_with_feudal_context, empirically_contingent).
narrative_ontology:cs_axiom('a438a642-5116-402e-a4b5-b6df95509233', secondary, ancient_law_cannot_bind_democratic_legislature).
narrative_ontology:cs_axiom_status(ancient_law_cannot_bind_democratic_legislature, holdable).
narrative_ontology:cs_axiom_grounding('a438a642-5116-402e-a4b5-b6df95509233', ancient_law_cannot_bind_democratic_legislature, conventional).
narrative_ontology:cs_reference_frame('a438a642-5116-402e-a4b5-b6df95509233', charter_as_extinct_baronial_compact).
narrative_ontology:cs_drift_state('a438a642-5116-402e-a4b5-b6df95509233', contemporary_post_hra_constitutional_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a438a642-5116-402e-a4b5-b6df95509233', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_industry).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_claim_litigants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, higher_law_jurists).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, legal_positivist_source_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Departments and ministers exercise prerogative and emergency powers bounded only by ordinary statute and political accountability. When a restraint claim invokes the charter, government counsel argue it is a historical document with no present force, and the argument succeeds. The executive gains a widening margin of unreviewable action; exit from the arrangement is unnecessary, since it can reshape the rules through its parliamentary majority whenever it wishes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments, agenda_setter).

% Holds the power to repeal the charter's surviving provisions or to entrench them beyond ordinary revision, and has done neither across three centuries. Each session confirms the arrangement by inaction: no ancient provision acquires special force, and no new fundamental law displaces ordinary procedure. Electoral timescales reward spending capital elsewhere; stepping outside the arrangement would mean binding future majorities, including rivals', for no certain return.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament, agenda_setter,
    institutional, biographical, arbitrage, national).

% Administers the line between history and law: when litigants ground arguments in charter provisions, judges treat the citations as rhetorical or confirmatory and dispose of the claims under ordinary doctrine. The bench gains coherence and doctrinal control from administering a single determinate source of authority, and it cannot step outside the arrangement without overturning the orthodoxy it is embedded in; individual judges age into it and retire within it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, senior_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Museums, trusts, and event producers stage anniversaries, exhibitions, and ceremonies around the charter's prestige. Their revenue depends on the document remaining famous, and fame is easiest to monetize when the text carries no obligations that could embarrass a sponsor or complicate a commemoration. Exit is easy — the brand is portable — but there is no incentive to leave.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, heritage_commemoration_industry, beneficiary,
    organized, biographical, mobile, national).

% Movements that organize around the charter as a people's guarantee of due process and lawful restraint find their central instrument legally inert: petitions citing it are received as correspondence, not law. Their membership's political identity is built on the charter's promise; abandoning it would mean dissolving the movement's reason to exist, so they continue invoking a text that no longer answers.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    organized, generational, identity_locked, national).

% Individuals who raise charter provisions in defense — against forfeiture, extradition, taxation, or administrative detention — bear the direct costs: struck arguments, adverse costs orders, sometimes findings of frivolousness. They arrive one at a time, without coordination, and their exit is simply to stop raising the argument, which most do after a first defeat.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_claim_litigants, payer,
    powerless, immediate, trapped, national).

% Judges and academics who seek to ground review of sovereign action in fundamental law lose their oldest warrant each time the charter is ruled out as a source of present obligation. Many redirect to human-rights statutes and common-law principles, which keeps them inside the profession but outside the tradition they trained for; pressing the charter case openly carries professional cost.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, higher_law_jurists, payer,
    moderate, generational, constrained, national).

% The wider public is schooled to regard the charter as the foundation of its liberties — the origin of jury trial, due process, and immunity from arbitrary punishment. When executive action strains against those expectations, they discover the document they were taught to venerate gives them no procedural handle. They have no seat in the courtroom or the legislature where the document's status is settled, and no practical exit from the constitutional culture that raised the expectation.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, charter_heritage_public, excluded,
    powerless, generational, constrained, national).

% Scholars who study the charter's drafting, reception, and afterlife see the whole structure at once: what the 1215 settlement actually promised, what the 1297 text preserved, how seventeenth-century politicians appropriated it, and how the modern arrangement draws on its prestige while disclaiming its force. They collect no share of the arrangement and bear none of its costs; their publications are the closest thing to an outside audit.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_governments).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles which instruments count as operative law: by confining the charter to its historical moment, the arrangement prevents a dead-hand medieval text and the contemporary legislative process from issuing competing commands, giving officials and courts a single determinate source of authority.
% TRANSFER_FUNCTION: Moves discretionary authority and legitimation capital upward to the executive (and derivatively to whichever faction commands Parliament), while moving the costs of hollowed guarantees — failed charter claims, deflated constitutional movements, eroded fundamental-law expectations — onto popular constitutionalists, charter-reliant litigants, and the wider venerating public.
% ABSENT_VOICES: The charter's original contracting parties are four centuries dead and cannot testify to what was promised; living popular constitutionalists appear only as losing litigants, never as participants in shaping the justiciability doctrine that defeats them; the heritage public taught to venerate the charter as their guarantee has no seat in the courtroom or the cabinet room where its non-bindingness is administered.
% DISAPPEARANCE_RATIONALE: If the obsolescence doctrine vanished overnight — if charter provisions became justiciable and binding on modern sovereign action — judicial review would expand immediately, executive recourse to prerogative and emergency power would contract, the parliamentary-sovereignty orthodoxy would face a fundamental-law limit for the first time since the seventeenth century, and every restraint claim currently dismissed as historical rhetoric would reopen. The executive seat loses its discretion premium; the victim seats gain an enforceable instrument.
% FOUNDING_PROBLEM: How a constitutional order can honor a venerated medieval restraint compact while operating sovereignty structures — Crown-in-Parliament, responsible government, positive-law jurisprudence — that cannot admit a superior, dead-hand law; the arrangement answers by reclassifying the charter as history rather than as law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional historians across the academic spectrum attest the charter's feudal, baronial character and its limited original scope; appellate dicta and the Law Commission's repeals work attest the near-total disappearance of operative charter provisions; and the consistent failure of charter-based claims in court attests the doctrine's operation. The heritage sector's own scholarship concedes the ceremonial gap. No beneficiary attestation is required for any of these facts.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.70: the transfer is systematic — every episode of prerogative or emergency action taken beyond ordinary-law limits is a unit of discretion the charter's promise would otherwise contest, and legitimation drawn from charter imagery during exactly such episodes is collected against a guarantee the same imagery disclaims. Suppression 0.58 is a raw structural measure (justiciability doctrine, costs exposure, precedent hierarchy) carrying a substantial internalized component — jurists pre-classify charter arguments as frivolous — and is authored unscaled, per the rule that only extractiveness is scaled by directionality and scope. Theater 0.78: the veneration apparatus (anniversaries, ceremonial copies, memorial architecture, curricula) dwarfs the operative function, which is reduced to confirmatory citations; the theater is not decorative but functional — it is the mechanism that keeps prestige harvestable while force stays disclaimed. Accessibility collapse 0.40: alternative restraint routes (human-rights statutes, common-law principles, political accountability) remain partly open, so understanding the arrangement collapses only the specific alternative of charter-as-fundamental-law. Resistance 0.45: revival attempts recur (clause-61-style campaigns, academic fundamental-law arguments, occasional sympathetic dicta) but are episodic, fragmented, and uncoordinated — the victim seats have not formed a coalition, and their fragmentation is itself part of the arrangement's stability. All three metric series run on one shared seven-point grid (interval units read as years, 1900 to 2020 at twenty-year steps); trajectories are monotonic rather than cyclical — veneration intensity and the veneration-force gap widen together while suppression stays nearly flat, indicating a stable enforcement picture beneath drifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat the arrangement presents as administrative clarity — one determinate source of law, no dead-hand interference; with arbitrage-grade exit (it writes the rules through its majority), the executive computes the mildest possible reading. From the trapped litigant seat the same arrangement presents as total foreclosure: the guarantee they were taught to invoke dissolves on contact with the courtroom. Parliament sits between: it is the custodian that could restore or bury the charter's force at any sitting and instead confirms the arrangement by inaction, bearing diffuse reputational cost while enjoying unqualified authority — the cost-asymmetry signature of an administrator who could change things but bears less cost from leaving them alone. The judiciary administers without collecting. Identity-lock concentrates on the popular-constitutionalist seat: the fusion is ideological — the charter's promise is constitutive of the movement's political identity, making exit unthinkable rather than merely costly — which raises their experienced severity above what organizational power alone would predict; if that identity frame broke, the victim set would thin as members migrated to statutory-entrenchment politics.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation toward the beneficiary pole: the executive sits nearest it (collects the discretion premium directly; arbitrage exit pushes it further out), Parliament mildly so (its benefit is the absence of a limit it could itself reimpose — a custodian's benefit, not a collector's), and the heritage industry incidentally (it monetizes veneration, which the arrangement leaves free-floating). Victim declarations drive the opposite pole: litigants sit nearest the full-target end (direct, repeated, unavoidable bearing of failed-claim costs; trapped exit), popular constitutionalists next (identity lock amplifies their effective position toward the target end beyond what organization alone implies), and higher-law jurists somewhat inside them (partial exit into statutory and common-law channels). The judiciary derives near-symmetric: it applies the arrangement without collecting from it. Operative seats carry national scope, which the engine scales modestly into effective extraction; suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The tempting misclassification is the atrophied-vestige reading: the function is visibly dead and the theater ratio is extreme. The receipt surface blocks it — the gains demonstrably accrue to a named seat (the executive's discretion premium), and a structure whose extraction lands in a capturer's hands stays capture-shaped regardless of how vestigial its original function looks. The mandatrophy question separates cleanly here: the charter's ORIGINAL mandate (resolving baronial-crown grievances over scutage, wardship, and forest privilege) is long dead — nobody proposes returning to it. But the doctrine erected on that grave performs live allocative work: it decides, freshly, in every restraint controversy, which instruments count as law. The founding problem of the ARRANGEMENT (how to honor a venerated restraint compact while operating sovereignty that cannot admit superior dead-hand law) is contested-live, recurring at each executive-overreach episode. So the arrangement is not a resolved mandate wearing old clothes; it is an operating allocation device whose cover story is a relic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the magna_carta_constraint_authority kernel — the feudal-obsolescence reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling stories: if the living-constitutionalism sibling computes a low-extraction live restraint on the same referent, the dispute reduces to a single structural element — whether the charter''s normative force survived its feudal context — on which the readings are exhaustive contradictories.',
    'Adopting the living-constitutionalism sibling would empty this story''s victim set (restraint claims succeed), convert the executive from beneficiary to target, and flip the type from snare toward rope; adopting the parliamentary-sovereignty sibling would relocate the operative authority into statutory revision politics and dissolve the charter-specific layer of this constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the Magna Carta authority kernel; disagreement located in whether normative force survived the feudal context.').

omega_variable(
    veneration_extraction_coupling,
    'Is the measured extraction a product of the obsolescence doctrine itself, or of the hybrid veneration-without-force arrangement that this reading explicitly rejects — such that the reading''s own cultural victory would deflate the extraction it measures?',
    'Track extraction across periods of differing veneration intensity: if harvest-side activity (strategic invocation of charter prestige during restraint controversies) falls when veneration wanes, the extraction rides on the hybrid rather than on the doctrine.',
    'If extraction rides on the hybrid, this reading is diagnostic rather than complicit, and the capture classification attaches to the veneration complex rather than to the obsolescence doctrine alone; if intrinsic, the doctrine is independently extractive regardless of what anyone believes about the charter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veneration_extraction_coupling, conceptual, 'Whether extraction is coupled to residual veneration the reading itself denies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of charter-based restraint claims structural (justiciability doctrine, costs exposure, precedent hierarchy) or internalized (jurists and litigants pre-classify charter arguments as frivolous and never raise them)?',
    'Post-barrier-lowering trajectory: examine charter-argument filing rates after any event that lowers the structural barrier (for example, a judgment entertaining a charter citation as load-bearing); if filings stay low despite the opened door, a substantial internalized component persists.',
    'If internalized, effective suppression exceeds the structural measure and the arrangement would outlive formal doctrine change; if structural, judicial or legislative reform could release suppressed claims quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of charter-based claims; rough split estimated 60% structural, 40% internalized.').

omega_variable(
    residual_clause_force,
    'Do the surviving charter provisions (notably clauses 39 and 40 of the 1297 text) retain any independent operative force in United Kingdom law, or are all modern citations merely confirmatory of ordinary common-law and statutory doctrine?',
    'Doctrinal audit of every modern judicial citation of the charter: classify each as load-bearing (the outcome would differ absent the citation) or confirmatory.',
    'Any load-bearing citation falsifies this reading''s core premise of zero binding authority and shifts the story toward the parliamentary-sovereignty sibling''s territory; universal confirmatory status confirms the authored epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_clause_force, empirical, 'Whether residual clauses carry independent force or only confirm ordinary law.').

omega_variable(
    doctrine_persistence_basis,
    'Does the arrangement''s persistence depend on active enforcement by the benefiting seats (courts dismissing, governments resisting entrenchment), or merely on inertia — would it decay without maintenance, or sustain itself?',
    'Enforcement audit: catalog instances where the arrangement was actively reaffirmed under challenge (dismissals of clause-61-style arguments, ministerial resistance to entrenchment proposals) versus periods of pure non-action, and test whether non-action alone sustains non-bindingness.',
    'If inertia suffices, the atrophied-vestige reading competes and the measured extraction should be re-read as passive residue; if active reaffirmation is load-bearing, the capture classification and the named gain-flow seat stand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_persistence_basis, empirical, 'Active enforcement versus inertia as the persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t0, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t20, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t40, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t60, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 80, 0.64).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t80, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 100, 0.72).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t100, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_tr_t120, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 120, 0.78).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(mc_feudal_obsolescence_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t0, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t20, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t40, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t60, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t80, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 100, 0.66).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t100, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_be_t120, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 120, 0.7).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc_feudal_obsolescence_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t0, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t20, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t40, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t60, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t80, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 100, 0.56).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t100, observed).
narrative_ontology:measurement(mc_feudal_obsolescence_su_t120, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 120, 0.58).
narrative_ontology:measurement_basis(mc_feudal_obsolescence_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Magna Carta's authority' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the kernel. This member (feudal obsolescence) authors epsilon for the standing venerate-without-force arrangement as the obsolescence reading sees it; the living-constitutionalism member authors epsilon for the same referent as a live inherited restraint (its victim and beneficiary sets invert this file's); the parliamentary-sovereignty member relocates the operative authority into statutory revision politics and dissolves the charter-specific layer. The upstream member by empirical confidence is this one — the historical record of the charter's feudal character and limited original scope is the best-evidenced layer — and it feeds the others: each sibling's plausibility depends on what this reading concedes or denies about the original compact. All three files link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
