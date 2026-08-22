% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution Reading: Text as Evolving Aspirational Framework
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The living-constitution reading interprets the U.S. Constitution as a
 *   framework whose meaning evolves with society's moral and social
 *   understanding. This is ONE READING of the contested kernel
 *   'us_constitution_1787'; the originalist and positivist readings
 *   instantiate different constraints from the same text. The living reading
 *   authorizes judges to recognize unenumerated rights (privacy, dignity,
 *   sexual autonomy, gender identity), to interpret enumerated powers
 *   expansively to modern circumstances (commerce clause, executive
 *   prerogative), and to shift constitutional meaning through doctrinal
 *   development rather than formal amendment. This generates extraction
 *   because (1) interpretive authority transfers from the text-as-fixed and
 *   the ratifying generation to contemporary judges and elite interpreters;
 *   (2) substantive rights claims are recognized without democratic
 *   supermajority ratification; (3) the 'evolving norms' frame is vulnerable
 *   to capture by whoever defines contemporary values. The reading is CLAIMED
 *   as tangled_rope (genuine coordination problem—keeping the Constitution
 *   applicable across centuries—AND asymmetric extraction—transferring
 *   meaning-setting power to the few). The metrics reflect this:
 *   extractiveness rises from 0 in 1787 (the reading did not exist; the text
 *   was intended as fixed) to 0.68 by 2026 (the living reading now dominates
 *   federal jurisprudence and powerfully shapes substantive law). Suppression
 *   rises as the machinery to defend this reading against originalist
 *   challenge intensifies. Theater rises as interpretive work becomes
 *   increasingly performative—judges and scholars describing what
 *   'contemporary values' demand while actually encoding elite preferences.
 *
 * KEY AGENTS:
 *   - progressive_judicial_coalition — institutional agenda-setter; sets living-constitution doctrine through appointment, precedent, and collegial influence; high extractive capture
 *   - civil_rights_advocates — organized beneficiary; leverage claims for unenumerated rights through the living reading's framework; moderate extractive benefit
 *   - expansionist_executive — institutional beneficiary; benefits from expansive reading of executive power and federal regulatory authority; moderate extractive benefit
 *   - originalist_legal_movement — institutional payer; excluded from agenda-setting, bears cost of marginalized doctrine, constrained exit
 *   - constitutional_stability_defenders — diffuse payer; bears cost of doctrinal instability and unpredictability, identity-locked to constitutional commitment
 *   - intellectual_elite_interpreters — powerful agenda-setter and beneficiary; defines what 'evolving norms' mean; arbitrage-grade exit options; high extractive capture
 *   - state_legislatures — powerful payer; loses policy sovereignty to living-reading expansions of federal power and individual rights; constrained exit
 *   - voter_public — powerless excluded; subject to constitutional rules as interpreted by others; trapped exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.72).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution Reading: Text as Evolving Aspirational Framework").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'a44f4d9f-f344-406d-bf06-b7b7dcd978a8').
narrative_ontology:cs_kernel_codification('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', fixed_text).
narrative_ontology:cs_authority_grounding('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', lineage).
narrative_ontology:cs_interpretation_layer_present('a44f4d9f-f344-406d-bf06-b7b7dcd978a8').
narrative_ontology:cs_reading_relation('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', constitutional_meaning_evolves_with_society, deontological).
narrative_ontology:cs_axiom('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', secondary, judicial_role_includes_contemporary_value_interpretation).
narrative_ontology:cs_axiom_status(judicial_role_includes_contemporary_value_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', judicial_role_includes_contemporary_value_interpretation, deontological).
narrative_ontology:cs_reference_frame('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', constitutional_meaning_as_living_framework).
narrative_ontology:cs_drift_state('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a44f4d9f-f344-406d-bf06-b7b7dcd978a8', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_judicial_coalition).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, expansionist_executive).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_legal_movement).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, constitutional_stability_defenders).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, narrow_interpretation_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, intellectual_elite_interpreters).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices (Warren, Brennan, Marshall, Stevens, Sotomayor, Kagan legacy) who adopted living-constitution methodology to expand rights protections and federal power. They set the doctrinal frame through majority opinions, precedent, and appointment influence. Their power rests on the bench; they cannot exit without losing judicial authority.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, progressive_judicial_coalition, agenda_setter,
    institutional, generational, constrained, national).

% Civil rights organizations, LGBTQ+ advocacy groups, reproductive rights organizations that benefit from judicial recognition of unenumerated rights through living-constitution doctrine. They depend on the courts for protection when legislatures are hostile. Their exit (accepting originalism) would require accepting that rights they claim are not constitutionally protected.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Presidents and executive agencies (EPA, OSHA, DOJ, HHS) that benefit from living-reading expansions of federal commerce power and executive prerogative. The reading enables broad interpretation of presidential and regulatory authority to address contemporary challenges (climate, public health, financial regulation). New administrations can shift executive posture; exit is mobile.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, expansionist_executive, beneficiary,
    institutional, biographical, mobile, national).

% Judges (Scalia, Thomas, Alito, Barrett), scholars (Barnett, McGinnis, Randy Barnett's students), and legal organizations (Heritage Foundation, Federalist Society) committed to original public meaning. They are structurally excluded from federal judicial majority opinion-setting; their power operates through appointment strategy and appellate dissent. Their identity is locked to originalism.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_legal_movement, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, originalist_legal_movement, excluded).

% Scholars and lawyers (Randy Barnett's constitutional legitimacy critique, the fixed-meaning school) who believe constitutional law requires written, fixed rules changed only through Article V. They pay through institutional marginalization and the cost of continuous legal contestation. They cannot exit without abandoning constitutional commitment itself.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_stability_defenders, payer,
    moderate, civilizational, identity_locked, national).

% Jurists and advocates (federalism scholars, property-rights groups, state sovereignty defenders) who want narrower readings of constitutional powers and protections. The living reading often expands federal authority and individual rights at their expense. They can organize politically, fund litigation, and support originalist judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, narrow_interpretation_adherents, payer,
    organized, biographical, mobile, national).

% The federal judiciary as an institution. The living reading maximizes institutional power by keeping interpretation open and courts as final arbiters. Judges as individual actors benefit from the prestige and authority of being constitutional interpreters. The hierarchy's institutional interest aligns with living-reading hegemony.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_courts_institutional_hierarchy, agenda_setter,
    institutional, generational, analytical, national).

% State legislatures whose policy space is constrained by living-reading expansions of federal power (commerce clause, Fourteenth Amendment) and individual rights (abortion, marriage, healthcare). They pay through lost sovereignty. They are excluded from the interpretive conversation; they learn the new constitutional rules from federal court decisions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_legislatures, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, state_legislatures, excluded).

% Law professors, appellate litigators, think tank scholars who set the frame for what 'contemporary values' and 'evolving norms' mean. The living reading empowers expertise-based interpretation; these actors define the norms and thus capture interpretive authority. High exit options (could switch to originalism if political winds shift); high benefit (career, prestige, institutional position).
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, intellectual_elite_interpreters, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, intellectual_elite_interpreters, agenda_setter).

% American voters and citizens subject to constitutional law as courts interpret it but with no formal seat at the table. They are excluded from the interpretive process. Article V amendment is available in theory but prohibitively costly in practice. They experience constitutional meaning as something courts impose rather than something they participate in setting.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, voter_public, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, intellectual_elite_interpreters).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative framework (the written Constitution) that remains applicable across centuries of social, technological, and moral change without requiring formal amendment each time interpretation shifts. Solves the coordination problem of maintaining a shared supreme law that evolves with the polity's self-understanding.
% TRANSFER_FUNCTION: Transfers interpretive authority from the text-as-fixed and the ratifying generation to contemporary judges and elite interpreters, from the dispersed many (Article V supermajority) to the concentrated few (nine justices, law school credentialed interpreters). Moves the power to define what rights and powers the Constitution protects from democratic amendment processes to unelected adjudicators.
% ABSENT_VOICES: State legislatures are excluded from defending their prerogatives against federal power expansions. Originalist jurists are excluded from setting the methodological frame and have constrained ability to affect outcomes. The voter-public is excluded from ratifying what 'evolving norms' mean for their own constitutional rights. Structural exclusion operates through appointment dynamics (presidents control which judges sit) and through the framing of interpretive expertise (only law school-credentialed thinkers are deemed competent constitutional interpreters).
% DISAPPEARANCE_RATIONALE: If the living-constitution reading disappeared overnight—if federal courts reverted to strict originalism or locked interpretation to the text as written—substantive constitutional law would contract sharply. Rights recognized as implicit (privacy, dignity, equal protection applied to gender and sexual orientation) would lose federal judicial protection unless protected by statute. Executive power would be constrained to enumerated authorities, reducing federal regulatory reach. The scope of federal commerce power would narrow. States would recover sovereignty over policy domains now preempted by living-reading expansions. The institutional equilibrium would shift from judicial supremacy to coordinate branches and federal-state federalism.
% FOUNDING_PROBLEM: The 1787 Constitution was written for a small republic of white male property owners; by the 20th century, the nation faced industrial complexity, technological transformation, mass democracy, and social movements (civil rights, women's liberation, LGBTQ+ recognition) the text did not anticipate. Formal amendment through Article V is nearly impossible (supermajority lock-in). Without interpretive evolution, the Constitution would become obsolete and unenforceable as written, unable to govern a modern polity. The living reading solves this by treating the text as an enduring framework that judges apply meaningfully to new circumstances, keeping it alive and relevant.
% FOUNDING_PROBLEM_CORROBORATION: Living-reading proponents (Brennan, Marshall, contemporary progressive scholars) argue the problem remains live: constitutional law must accommodate modern realities (digital privacy, biomedical autonomy, social inclusion) or the document loses authority and judges lose legitimacy. Originalists counter that the founding problem was substantially solved in 1868 (Fourteenth Amendment) and remains solvable through Article V; the living reading treats political amendment-resistance as constitutional necessity, thus conflating difficulty with impossibility. Comparative scholars note that other democracies (Canada, Israel) recognize unenumerated rights through written texts without living-reading doctrine; the claim that the reading is necessary is contested. Legal historians document that many living-reading victories (civil rights, gender equality, marriage autonomy) preceded or paralleled statutory and common-law shifts, suggesting the reading is a vehicle for rights already gaining social acceptance rather than the cause of that acceptance.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the living reading transfers interpretive authority from the dispersed (Article V ratification, original intent) to the concentrated (nine justices, credentialed scholars). At t=1787, extractiveness is 0 because the reading did not exist—the Constitution was intended as a fixed text that could only be changed through amendment. By t=1954 (Brown v. Board, reinterpreting equal protection), the reading had gained institutional traction, and extractiveness was measurable (0.38). By t=1973 (Roe v. Wade, recognizing unenumerated privacy right), extractiveness reached 0.52 as judges began recognizing rights nowhere in the text. By t=2026, extractiveness is 0.68 as living-constitution doctrine dominates federal jurisprudence, environmental law, administrative law, and civil rights doctrine. Suppression is high and rising because defending this reading requires active exclusion of originalism from elite judicial and intellectual spaces, appointment strategy that favors progressive judges, and interpretive gatekeeping by law schools and appellate litigators. Theater is moderate-high (0.58) and rising because a growing share of 'evolving norms' talk is performance—judges and scholars describing what society demands while actually encoding particular value commitments (elite preferences on sexuality, family structure, economic regulation) as universal principles. The time series shows the constraint accumulating extraction over centuries: it began as a latent alternative to original-intent reading and became hegemonic in the federal judiciary by century's end.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (progressive coalition, civil rights advocates, intellectual elite) perceive this as solving a genuine coordination problem: how to keep the Constitution alive and responsive without impossible amendment. Their extraction is experienced as legitimate interpretation of enduring principles to new circumstances. The payer seats (originalists, stability defenders, state legislatures) perceive the same structure as judicial usurpation and elite preference-laundering disguised as constitutional necessity. Originalists argue the founding problem was solved in 1868 (Fourteenth Amendment) and Article V remains available; the living reading's treatment of amendment as prohibitively costly is analytically choosing constraint over solution. State legislatures perceive lost sovereignty as the cost of living-reading expansions of federal power. The voter-public, excluded entirely, experiences the constraint as the outcome of processes they do not participate in and cannot overturn short of Article V (prohibitively costly). The engine should compute different types at different seats: from the progressive coalition's position (setter + beneficiary, institutional power), the reading may compute as rope or tangled_rope with positive payoff. From the originalist position (excluded institutional payer, constrained exit), it computes as snare. From the voter-public position (powerless, trapped, excluded), it computes as snare or piton depending on whether the extraction is actively maintained through suppression or held by inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive judicial coalition: d ≈ 0.15 (strong beneficiary, institutional power, arbitrage-grade exit if they switched reading, but they do not; the reading benefits them more than originalism would). Civil rights advocates: d ≈ 0.25 (moderate beneficiary, organized power, constrained exit because they depend on judicial recognition of their claims). Expansionist executive: d ≈ 0.20 (beneficiary of expansionist reading of executive power, institutional player, mobile exit). Originalist legal movement: d ≈ 0.85 (payer through exclusion from meaning-setting, institutional power but constrained to appointment strategy, identity-locked to commitment to original meaning). Constitutional stability defenders: d ≈ 0.78 (payer through doctrinal instability, moderate power, identity-locked to written constitutionalism such that exit requires abandoning core commitment). Narrow interpretation adherents: d ≈ 0.72 (payer through expanded federal/individual rights, organized power but mobile exit through political mobilization). State legislatures: d ≈ 0.80 (payer through loss of policy sovereignty, powerful but constrained by supremacy clause). Intellectual elite interpreters: d ≈ 0.08 (strong beneficiary—their expertise and prestige are maximized by a reading that requires expert interpretation; high arbitrage exit). Voter-public: d ≈ 0.92 (nearly complete target; trapped, excluded, powerless, subject to constraints they do not participate in setting).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (keeping an 18th-century text applicable to a 20th-century polity) was genuinely live from 1870–1973, during which the living reading filled a coordination gap. However, the problem may be dead as of 2026: modern civil rights are now anchored in statute (Civil Rights Act, Americans with Disabilities Act, statutory protections), international human rights law, and cultural norm-shift such that the reading's claim to solve the coordination problem is increasingly backstory rather than current function. What persists is the extraction machinery—the institutional power arrangement that gives judges and elite interpreters control of constitutional meaning—and the theater required to defend it (the continuous performance that 'evolving values' demand what judges happen to decide). The mandatrophy signal: founding_problem_status='contested' + disappearance_verdict='world_rearranges' suggests the arrangement would reorganize if the constraint disappeared, but whether that reorganization would reproduce the living reading or shift toward originalism/amendment depends on political dynamics not captured by the constraint alone. The reading's vulnerability to capture is high: whoever defines 'contemporary values' and 'evolving norms' wields extraordinary power, and that power naturally concentrates in elite interpreters with institutional access. The reading offers no structural protection against elite preference-laundering under the guise of universal principles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_norms_capture_risk,
    'Who defines ''evolving norms'' and ''contemporary values'' for the purpose of constitutional reinterpretation? Is there structural protection against elite preference-laundering disguised as universal principles?',
    'Comparative analysis of judicial doctrine over time: track correlation between judges'' ideological priors (determined by appointment and previous record) and their constitutional conclusions about what ''values'' the contemporary moment demands. High correlation would indicate preference-laundering; low correlation would indicate genuine deference to external standards.',
    'If capture is demonstrated, the reading shifts from tangled_rope (genuine coordination + asymmetric extraction) to snare (extraction with coordination as cover story). The legitimacy of the entire reading depends on its impermeability to capture; if permeable, it is a mechanism for judges to impose their values under constitutional guise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_norms_capture_risk, empirical, 'Whether ''evolving norms'' is an external constraint on judges or a label for judges'' preferences.').

omega_variable(
    amendment_prohibitive_cost_claim,
    'Is Article V amendment truly prohibitively costly, or does the living reading treat political difficulty as constitutional necessity?',
    'Historical comparison: measure the actual cost and time required for formal amendments (e.g., 15th Amendment, 19th Amendment, 26th Amendment) and compare to the living reading''s claim that amendment is impossible. Test whether the reading''s foundational justification (amendment is blocked) rests on structural impossibility or on political alignment favoring interpretation over amendment.',
    'If amendment is not structurally impossible but merely politically difficult (costly at current political prices), the founding problem is not solved by the living reading but rather the reading is a solution to political resistance to formal amendment. The constraint would then be better characterized as a mechanism for circumventing democratic supermajority requirements, shifting the classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_prohibitive_cost_claim, empirical, 'Whether amendment is structurally impossible or politically difficult and interpreted as impossible.').

omega_variable(
    original_meaning_foreclosure_vs_coexistence,
    'Does the living reading logically foreclose the originalist reading, or do they coexist as different methodologies that could both be valid within different domains or institutions?',
    'Formal logical analysis: if the living reading''s core axiom (meaning evolves; framers'' intent is not binding) directly contradicts the originalist axiom (meaning fixed at ratification; framers'' intent is binding), they foreclose each other. If they are compatible as different interpretive methodologies (e.g., one valid for certain clauses, the other for others), they coexist.',
    'If they foreclose each other, the reading_relations entry for originalist_reading should be ''forecloses'' rather than ''coexists_with''. If they coexist, the network relationship is one of institutional competition for hegemony rather than logical contradiction. The distinction affects how the engine models the constraint''s resilience to challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_meaning_foreclosure_vs_coexistence, conceptual, 'Whether the living and originalist readings are logically incompatible or compatible as different methodologies.').

omega_variable(
    civil_rights_gains_attribution,
    'Would the civil rights gains recognized through living-constitution doctrine (privacy, sexual orientation, gender identity) have been achieved through other mechanisms (statute, common law, social norm shift) even without the reading? How much of the beneficiary''s gain is attributable to the reading versus other forces?',
    'Counterfactual analysis: compare jurisdictions with different constitutional methodologies (originalist, living, text-based); examine whether civil rights protections correlate with constitutional reading or with political/legislative alignment. Historical tracing: document whether rights were recognized through statute before or after judicial recognition.',
    'If civil rights gains would have been achieved anyway through statute and social norm shift, the living reading''s beneficiary status is overstated and it becomes less clearly a solution to the coordination problem of making the Constitution applicable. If civil rights gains depend crucially on judicial recognition via living-reading, the reading is a primary vehicle for rights protection and its extraction is more defensible as the necessary cost of rights recognition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_rights_gains_attribution, empirical, 'Whether the reading is the cause or the vehicle for civil rights recognition.').

omega_variable(
    reader_authority_derivation_ambiguity,
    'Where does judicial authority to reinterpret the Constitution derive from in the living reading? Does it rest on legitimate constitutional grounds (implied by the text, necessary to its function), or is it self-asserted authority grounded in institutional power?',
    'Textual analysis: examine the living reading''s canonical justifications (Marshall''s Marbury v. Madison precedent, the structure of the Constitution as supreme law, the need for courts to say what the law is). Determine whether these textual anchors support living-reading methodology or whether they have been extended beyond their original scope to justify the reading.',
    'If reader authority is textually grounded, the reading is structurally legitimate. If it is self-asserted institutional power, the reading''s extraction becomes less defensible—it would be using constitutional authority to expand constitutional authority, a circular legitimacy claim. This affects whether the reading is classified as tangled_rope (legitimate coordination + extraction) or snare (extraction with authority-claims as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_authority_derivation_ambiguity, conceptual, 'Whether judicial authority to reinterpret derives from textual/structural grounds or institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__living_reading, theater_ratio, 1787, 0.08).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_1787__living_reading, theater_ratio, 1868, 0.14).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.32).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_1787__living_reading, theater_ratio, 1973, 0.48).
narrative_ontology:measurement(us_c_tr_t2003, us_constitution_1787__living_reading, theater_ratio, 2003, 0.54).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_1787__living_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__living_reading, base_extractiveness, 1787, 0.0).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_1787__living_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.38).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_1787__living_reading, base_extractiveness, 1973, 0.52).
narrative_ontology:measurement(us_c_be_t2003, us_constitution_1787__living_reading, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_1787__living_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__living_reading, suppression_requirement, 1787, 0.05).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_1787__living_reading, suppression_requirement, 1868, 0.12).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_1787__living_reading, suppression_requirement, 1973, 0.58).
narrative_ontology:measurement(us_c_su_t2003, us_constitution_1787__living_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_1787__living_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.22).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, federal_judicial_authority_defense).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, unenumerated_rights_recognition).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, federal_power_expansion).

% DUAL FORMULATION NOTE:
% The living reading is one of three structurally distinct readings of the same constitutional kernel. The originalist reading produces a much narrower constraint (lower extractiveness, narrower beneficiary set, higher accessibility of alternatives through originalist adjudication). The positivist reading produces a middle constraint (text-constrained interpretation, formal amendment as the legitimate change mechanism). These three stories form a constraint family linked by common kernel; each has its own ε, its own beneficiary/victim structure, its own classification. The living reading is the currently hegemonic reading in federal courts, which makes it the baseline for understanding how constitutional authority operates in 2026.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
