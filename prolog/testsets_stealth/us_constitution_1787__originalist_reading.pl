% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading: U.S. Constitutional Meaning Fixed at Ratification
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_1787 (the U.S. Constitution as authority structure): the
 *   originalist reading, under which constitutional meaning was fixed at
 *   ratification and the framers' enactment binds interpreters. The standing
 *   arrangement under contest is the federal interpretive order governed by
 *   that standard: a judiciary that must locate authority in 1791/1868
 *   enactment-era meaning, an evidentiary apparatus of founding-era
 *   scholarship, a professional movement that staffs the bench, and a
 *   designated change route (Article V) that is formally open and practically
 *   prohibitive. Under the fixed standard, founding-era practices acquire
 *   legitimacy they lack under an evolved-meaning standard, and modern
 *   social-rights claims sit outside the constraint boundary. Per the
 *   ε-invariance principle, the kernel decomposes into three readings
 *   (originalist, living, positivist) that are separate constraints with
 *   separate ε, beneficiaries, and victims; this file authors only the
 *   originalist reading, with the siblings as linked constraints. Two
 *   authoring assumptions are stated explicitly: (1) claimed_type 'rope'
 *   records the READING'S OWN FRAMING (fixed meaning as the coordination that
 *   makes constitutional law and democratic self-government possible),
 *   mirroring the one-shot example's claim-as-beneficiary-framing pattern;
 *   the metrics are authored independently as descriptive values, and the
 *   divergence is the measurement. (2) ε is reading-indexed per the
 *   kernel-referent rule: it is what the originalist reading's own lights
 *   acknowledge as cost, not what the living reading alleges.
 *
 * KEY AGENTS:
 *   - originalist_supreme_court_majority: agenda-setter (institutional/identity_locked) — administers the fixed-meaning standard, decides what founding evidence binds
 *   - originalist_legal_movement: primary beneficiary (organized/mobile) — collects seats, clerkships, legitimacy, doctrinal wins; staffs the enforcement pipeline
 *   - gun_rights_advocates: aligned beneficiary (organized/mobile) — doctrinal wins became available only under ratification-fixed meaning
 *   - state_sovereignty_advocates: aligned beneficiary (organized/constrained) — collect enforceable federalism limits
 *   - modern_unenumerated_rights_claimants: primary target (powerless/trapped) — claims ruled outside the constraint boundary; designated exit practically closed
 *   - historically_excluded_groups: structural target (organized/constrained) — equal-citizenship claims routed to amendment or contested textual anchors
 *   - living_constitutionalist_jurists_scholars: excluded voice (moderate/identity_locked) — the suppressed alternative, out of the federal conversation
 *   - constitutional_historians: analytical observer — sees the gap between professional historiography and the constraint's evidentiary practice
 *   - independent_state_courts: excluded with live exit (institutional/mobile) — the one operative alternative forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.3).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading: U.S. Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'f93b8df3-2366-4de5-9c35-f298e3517238').
narrative_ontology:cs_kernel_codification('f93b8df3-2366-4de5-9c35-f298e3517238', fixed_text).
narrative_ontology:cs_authority_grounding('f93b8df3-2366-4de5-9c35-f298e3517238', lineage).
narrative_ontology:cs_interpretation_layer_present('f93b8df3-2366-4de5-9c35-f298e3517238').
narrative_ontology:cs_reading_relation('f93b8df3-2366-4de5-9c35-f298e3517238', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('f93b8df3-2366-4de5-9c35-f298e3517238', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('f93b8df3-2366-4de5-9c35-f298e3517238', foundational, constitutional_meaning_fixed_at_enactment).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_enactment, holdable).
narrative_ontology:cs_axiom_grounding('f93b8df3-2366-4de5-9c35-f298e3517238', constitutional_meaning_fixed_at_enactment, conventional).
narrative_ontology:cs_axiom('f93b8df3-2366-4de5-9c35-f298e3517238', foundational, article_v_exclusive_legitimate_change).
narrative_ontology:cs_axiom_status(article_v_exclusive_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('f93b8df3-2366-4de5-9c35-f298e3517238', article_v_exclusive_legitimate_change, conventional).
narrative_ontology:cs_reference_frame('f93b8df3-2366-4de5-9c35-f298e3517238', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('f93b8df3-2366-4de5-9c35-f298e3517238', contemporary_post_2018_court, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f93b8df3-2366-4de5-9c35-f298e3517238', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, gun_rights_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, state_sovereignty_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, historically_excluded_groups).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, ratification_era_public_meaning_determinacy).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, article_v_exclusive_amendment_legitimacy).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_countermajoritarian_answer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A bloc of life-tenured justices appointed through a forty-year pipeline that selected for commitment to ratification-era meaning. They decide which founding-era evidence binds, write the opinions that apply it, and control which precedents survive. Their standing inside the legal movement that produced them depends on continuing fidelity to the method; a justice who abandons it repudiates the legitimacy narrative that justified their own appointment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_supreme_court_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% A network of lawyers, academics, and advocacy organizations built since 1982 to supply founding-era scholarship, staff the judiciary, and litigate test cases. It collects judicial seats, clerkship placements, amicus influence, and doctrinal wins, and it staffs the enforcement side by vetting nominees and training each new cohort. Its professional skills would transfer to whichever legitimacy framework next controls the bench, so its position is comfortable but not irreplaceable.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_legal_movement, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_legal_movement, agenda_setter).

% Litigants and advocacy organizations whose core objective, an individual right to keep and bear arms outside militia service, was unobtainable under mid-century doctrine and became achievable once the operative meaning was fixed at 1791. They defend the doctrinal wins and supply the historical research that extends them to new regulations.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, gun_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% State governments and their legal representatives, who gain enforceable limits on federal power when the commerce and enforcement clauses are read as ratified. They litigate to hold those limits and lose ground when the reading shifts toward modern national power; their alternative forum, Congress, is the institution the limits bind.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, state_sovereignty_advocates, beneficiary,
    organized, generational, constrained, national).

% Litigants pressing claims to privacy, autonomy, bodily integrity, and dignity that find no anchor in 1791 or 1868 usage. Their claim must either locate an enactment-era anchor, often unavailable, or lose. The designated alternative, persuading thirty-eight states to amend, is formally open and practically out of reach within a lifetime. They appear before the bench one case at a time and hold no seat in the standard-setting conversation that decides what counts as a constitutional claim at all.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Groups the 1787 and 1791 framings did not count as full participants: enslaved people and their descendants, women, the unpropertied. Their equal-citizenship claims must route through the Fourteenth Amendment's enactment-era meaning, through formal amendment, or through politically contested textual anchors, rather than through appeals to how constitutional values have matured since ratification. Organized civil-rights capacity gives them more leverage than individual litigants, but the routes open to them are the ones the standard itself designates.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historically_excluded_groups, payer,
    organized, generational, constrained, national).

% Judges and law professors whose account of constitutional authority, that meaning evolves with the society it governs, held the federal bench for two generations and now holds none of it. They write from state courts, dissent seats, and the academy; the hiring and clerkship pipelines that once carried them now select against them. Their professional identity is constituted by the method they are excluded for, and their objection is that the fixed-meaning account is itself an interpretive choice rather than a discovery.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_jurists_scholars, excluded,
    moderate, generational, identity_locked, national).

% Professional historians of the founding era whose archival findings are the constraint's evidentiary base. They watch their scholarship cited selectively, the practice they call law office history, and note that demands for determinate ratification-era answers routinely exceed what the record supports. Their professional standing does not depend on the constraint, so they can see and say what the seats inside it cannot.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    moderate, generational, analytical, national).

% State supreme courts that increasingly decide rights questions under their own constitutions' independent grounds. They are the one live institutional exit from the federal fixed-meaning standard, and their divergence grows as the federal standard narrows. They are not seated in the federal standard-setting conversation their divergence responds to, but unlike the excluded jurists they retain an operative forum.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, independent_state_courts, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single shared referent for all constitutional argument, the enacted text with its ratification-era public meaning, so that judges, lawyers, and citizens argue from the same authoritative object; binds unelected judges to enacted law rather than their own values; and routes all legitimate constitutional change through the Article V amendment process where the public deliberates directly. It also confers legitimacy on founding-era practices that an evolved-meaning standard would leave exposed.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges and contemporary majorities to the ratification-era enactors; moves decision-power over modern rights claims from courts to the political coalitions that control appointments and amendment; and moves doctrinal wins, judicial seats, and professional legitimacy to the lawyers and movements aligned with ratification-era meaning.
% ABSENT_VOICES: Living-constitutionalist jurists and scholars are excluded from the federal bench and selected against in the hiring and clerkship pipelines; unenumerated-rights claimants appear only as case-by-case losers, never as participants in setting the standard; professional constitutional historians whose findings complicate the evidentiary base are consulted selectively rather than seated. Independent state courts feel the standard's reach without a seat in its administration.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning constraint vanished overnight, the federal bench would reorganize around competing legitimacy sources, the appointment wars would lose their organizing principle, doctrine resting on ratification-era meaning (the individual gun right, limits on federal commerce power) would reopen, and the professional infrastructure built to supply founding-era evidence would lose its function. The originalist legal movement's institutional position would collapse within a generation.
% FOUNDING_PROBLEM: The countermajoritarian difficulty: an unelected judiciary applying an eighteenth-century text to modern disputes required an account of why its decisions bind. Originalism answered by fixing meaning at ratification, making judges servants of the enactors rather than authors. The deeper upstream problem is the one the 1787 text itself solved: collective-action failure among the states under the Articles of Confederation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the movement document the constraint's deliberate construction between 1971 and 1985 (Bork's 'Neutral Principles,' Meese's 1985 Attorney General speech, the Federalist Society's founding), attesting that the founding problem as articulated is a late-twentieth-century rationalization rather than a discovery. Dissenting justices and the living-reading academy attest the legitimacy problem is solved differently, not unsolved. The movement's own beneficiaries attest it as live and permanent. No attestation is fully outside contestation, but the constructed-genealogy documentation is the strongest corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).
:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε = 0.30 is reading-indexed: it is what the originalist reading's own lights acknowledge as the constraint's cost — the indeterminacy of founding-era evidence, the law-office-history capture its own self-critical members document, and the occasional unjust-but-constitutional outcome — assessed from inside as the price of legality rather than as extraction. The remaining metrics are authored independently as descriptive values: suppression 0.58 because persistence depends on active methodological enforcement (a forty-year appointments pipeline, canon control in hiring and clerkships, delegitimization of the rival method) rather than voluntary uptake; theater_ratio 0.32 because the historical-linguistic function is real but a growing share of founding-era argument is outcome-motivated; accessibility_collapse 0.52 because alternatives have largely collapsed inside the federal bench while persisting in the academy, dissents, and independent state constitutionalism; resistance 0.62 reflecting four decades of sustained scholarly and judicial opposition. The measurement series run on one shared grid (1971-2025, eight points, every tracked metric authored at every point). All three series rise: extraction-from-own-lights grows with dominance (an opposition method constrains; a dominant one channels), theater grows as outcomes align with appointing coalitions, and suppression_requirement traces the deliberate enforcement build-up from 1971 to its 2018 plateau. Suppression here is structural (appointment lock-in, canon control), not internalized, though the movement's own identity lock has an internalized component; the omega appointment_lock_in_persistence covers reversibility. Coordination type is identity_coordination because the constraint's dominant function is coordinating the legal profession's interpretive identity — who counts as a legitimate constitutional interpreter; the identity framing is not cover, since the shared-standard function is genuine and the payers' costs concentrate precisely where identity enforcement operates. Fixing cost is prohibitive: removal would require reversing three generational appointment cohorts and reopening recently settled doctrine, a cost no seat that could fix it can bear relative to the benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the constraint is law itself: the majority experiences fixed meaning as fidelity rather than burden, and its identity lock (appointed as originalists, confirmed on originalist testimony) makes the method constitutive of professional selfhood; from that seat the computed type should sit near rope. From the trapped rights-claimant seat the same structure is a closed door with a prohibitively expensive official exit; from that seat the computed type should sit near snare. The excluded jurist seat experiences the arrangement as illegitimate exclusion of a live alternative; the historian seat sees an evidentiary standard routinely outrunning its own record. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (originalist_legal_movement, gun_rights_advocates, state_sovereignty_advocates) derive low directionality: the standard subsidizes their litigation objectives and professional position. Victims derive high directionality: modern_unenumerated_rights_claimants are trapped (the designated exit, Article V, is practically closed), and historically_excluded_groups are constrained to the routes the standard itself designates. The agenda-setter is dual-positioned (administers and collects) and identity-locked, pinning it near the beneficiary end despite its formal neutrality role. Because the base ε is reading-indexed low, victim-seat effective extraction is amplified from a modest base; the gap between the reading's self-assessment and the victim-seat computation is precisely the quantity this story exists to measure. No directionality overrides were needed: the beneficiary/victim/exit data derives the seat directionalities directly. Note on coalition power: the constraint's answer to victim coalition-energy is routing — it directs coalitions to the amendment process, where the thirty-eight-state threshold makes success prohibitive; that routing is part of the structure, not a neutral feature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, binding an unelected judiciary to enacted law, is contested-live: originalists attest it permanent; the constructed-genealogy record (1971-1985) supports a rationalization reading. Mislabeling risk runs both ways. A pure-snare reading erases the constraint's real binding force: originalist method has constrained its own holders when enactment-era meaning cut against appointing-coalition preferences (flag-burning protection, criminal-confrontation rights), which a pure extraction story cannot explain. A pure-rope reading erases the asymmetric geometry: identifiable payers, a suppressed alternative, and a designated exit whose practical closure is built in. The honest classification keeps the coordination function and the payers in one frame. The R5 mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: the problem's liveness is disputed, but the arrangements' dependence on the constraint is not, so no zombie flag and no mandatrophy resolution fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of kernel us_constitution_1787. Which structural elements do the sibling readings (living_reading, positivist_reading) contest, and what would adopting a sibling change about the constraint boundary?',
    'Comparative classification across the three sibling constraint stories: the living reading widens the constraint set and readmits modern social-rights claims; the positivist reading drops the historical-evidence apparatus while keeping text-plus-amendment.',
    'If the positivist sibling were operative, the epistemic demands and the historiographic capture channel drop out and the victim set narrows to text-unanchored claims; if the living sibling were operative, the victim set inverts, with fixed-meaning-aligned litigants becoming the disfavored class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel, which reading, where the disagreement between readings is located.').

omega_variable(
    framer_intent_vs_public_meaning,
    'The reading''s label binds ''framers'' intent,'' but the operative doctrine since the 1990s is ratification-era PUBLIC meaning (new originalism) — which version is the binding constraint?',
    'Doctrinal analysis of post-2018 majority opinions: whether they consult enactment-era public usage or the subjective intentions of the drafters and ratifiers.',
    'If framers''-intent is binding, the evidence problem worsens (intent-assembly is more indeterminate than public usage) and the historiographic capture channel widens, raising both ε and theater_ratio; if public meaning is binding, the label is stale and the constraint is more determinate than its own name suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framer_intent_vs_public_meaning, conceptual, 'Internal ambiguity in the reading''s own formulation: intent originalism versus public-meaning originalism.').

omega_variable(
    law_office_history_share,
    'What share of the constraint''s founding-era evidentiary operation is outcome-motivated (law office history) rather than professionally sound historiography?',
    'Audit of the historical claims in majority opinions against the professional consensus of founding-era historians; citation-network analysis of movement scholarship.',
    'A high motivated share means theater_ratio is understated at 0.32 and the evidentiary base functions as advocacy infrastructure, shifting victim-seat classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_office_history_share, empirical, 'Prevalence of motivated founding-era argument in the constraint''s operation.').

omega_variable(
    constructed_genealogy_legitimacy,
    'Is the fixed-meaning constraint a discovered feature of legality (as the reading''s constitutive-of-law rhetoric claims) or a constructed political instrument (as its deliberate 1971-1985 build-out suggests), and does construction defeat the legitimacy claim?',
    'The historical record of the movement''s construction is settled; the open question is jurisprudential — whether a constraint''s deliberate construction defeats its claim to bind as law. Resolution requires legal-theoretical analysis, not further archival work.',
    'If construction is dispositive, the mountain-flavored self-presentation is cover and the constraint classifies as constructed coordination with identifiable beneficiaries, strengthening tangled-rope and snare readings at victim seats; if not, the rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_genealogy_legitimacy, conceptual, 'Naturalness ambiguity: discovered legality versus constructed instrument.').

omega_variable(
    article_v_route_accessibility,
    'Is the Article V amendment route — the constraint''s designated exit for those it rules against — genuinely open, or formally open and practically closed?',
    'Comparative amendment-success data: twenty-seven amendments in over two centuries, none via the convention route, and the modern success rate for rights-expanding amendments.',
    'If the route is practically closed, victim exit_options ''constrained'' should read ''trapped,'' effective suppression at victim seats is higher than the 0.58 scalar suggests, and the constraint''s central legitimacy argument (change remains available to the people) weakens correspondingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_v_route_accessibility, empirical, 'Whether the designated change route is a real alternative.').

omega_variable(
    appointment_lock_in_persistence,
    'Is the exclusion of the rival interpretive method from the federal bench reversible through ordinary political turnover, or self-perpetuating through generational appointment lock-in?',
    'Track appointment cohorts and methodological turnover across election cycles; compare against historical episodes of interpretive-regime change such as the Lochner-to-New-Deal transition.',
    'If lock-in is self-perpetuating, the exclusion is structural and durable, raising effective suppression at victim and excluded seats; if reversible, the constraint is better read as a contingent political equilibrium held in place by ordinary politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appointment_lock_in_persistence, empirical, 'Reversibility of the methodological exclusion machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_1787_originalist_tr_t1971, us_constitution_1787__originalist_reading, theater_ratio, 1971, 0.08).
narrative_ontology:measurement(us_const_1787_originalist_tr_t1979, us_constitution_1787__originalist_reading, theater_ratio, 1979, 0.12).
narrative_ontology:measurement(us_const_1787_originalist_tr_t1987, us_constitution_1787__originalist_reading, theater_ratio, 1987, 0.16).
narrative_ontology:measurement(us_const_1787_originalist_tr_t1995, us_constitution_1787__originalist_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(us_const_1787_originalist_tr_t2003, us_constitution_1787__originalist_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(us_const_1787_originalist_tr_t2011, us_constitution_1787__originalist_reading, theater_ratio, 2011, 0.27).
narrative_ontology:measurement(us_const_1787_originalist_tr_t2018, us_constitution_1787__originalist_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(us_const_1787_originalist_tr_t2025, us_constitution_1787__originalist_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(us_const_1787_originalist_be_t1971, us_constitution_1787__originalist_reading, base_extractiveness, 1971, 0.1).
narrative_ontology:measurement(us_const_1787_originalist_be_t1979, us_constitution_1787__originalist_reading, base_extractiveness, 1979, 0.13).
narrative_ontology:measurement(us_const_1787_originalist_be_t1987, us_constitution_1787__originalist_reading, base_extractiveness, 1987, 0.17).
narrative_ontology:measurement(us_const_1787_originalist_be_t1995, us_constitution_1787__originalist_reading, base_extractiveness, 1995, 0.21).
narrative_ontology:measurement(us_const_1787_originalist_be_t2003, us_constitution_1787__originalist_reading, base_extractiveness, 2003, 0.24).
narrative_ontology:measurement(us_const_1787_originalist_be_t2011, us_constitution_1787__originalist_reading, base_extractiveness, 2011, 0.27).
narrative_ontology:measurement(us_const_1787_originalist_be_t2018, us_constitution_1787__originalist_reading, base_extractiveness, 2018, 0.29).
narrative_ontology:measurement(us_const_1787_originalist_be_t2025, us_constitution_1787__originalist_reading, base_extractiveness, 2025, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(us_const_1787_originalist_su_t1971, us_constitution_1787__originalist_reading, suppression_requirement, 1971, 0.15).
narrative_ontology:measurement(us_const_1787_originalist_su_t1979, us_constitution_1787__originalist_reading, suppression_requirement, 1979, 0.25).
narrative_ontology:measurement(us_const_1787_originalist_su_t1987, us_constitution_1787__originalist_reading, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement(us_const_1787_originalist_su_t1995, us_constitution_1787__originalist_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(us_const_1787_originalist_su_t2003, us_constitution_1787__originalist_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(us_const_1787_originalist_su_t2011, us_constitution_1787__originalist_reading, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(us_const_1787_originalist_su_t2018, us_constitution_1787__originalist_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(us_const_1787_originalist_su_t2025, us_constitution_1787__originalist_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Constitution constrains government' covers three structurally distinct constraints: originalist (meaning fixed at ratification; high epistemic demands; modern social-rights claims outside the boundary), living (meaning evolves; text aspirational), and positivist (meaning is text plus democratic amendments). Per the ε-invariance principle these are separate stories with separate ε, beneficiaries, and victims, linked as one kernel family. The originalist reading is currently upstream in institutional power: it exerts structural pressure on the positivist sibling (positivists increasingly adopt enactment-era semantics to remain relevant) and logically forecloses the living sibling's core premise within any single framework, since fixed-at-ratification meaning and extra-amendamental evolution cannot both bind in one account.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
