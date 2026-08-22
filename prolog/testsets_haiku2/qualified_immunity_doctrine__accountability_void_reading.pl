% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine: Accountability Void Reading
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   The qualified immunity doctrine is a judicially created exception to
 *   civil liability for government officials, codified in case law (Harlow v.
 *   Fitzgerald, 1982) that systematically forecloses damages remedies for
 *   constitutional violations by police officers. Under this reading — the
 *   accountability void reading — the doctrine operates as a snare: it
 *   extracts compliance and authority from violation survivors by eliminating
 *   the civil remedy that would otherwise provide deterrence and
 *   accountability, while shielding officers from personal consequences. The
 *   constraint is characterized by near-total extraction (0.89) because
 *   violations occur with near-zero personal liability for the officer;
 *   suppression (0.91) reflects the mechanism's completeness — a plaintiff
 *   must prove the violated right was 'clearly established,' a moving target
 *   that retroactively narrows what counts as established law. Theater (0.62)
 *   reflects that the doctrine is defended through elaborate doctrinal
 *   reasoning (qualified immunity is justified as necessary for vigorous law
 *   enforcement) that increasingly disconnects from the doctrine's actual
 *   operation (the vast majority of summary judgments in qualified immunity
 *   cases protect officers from even trial, foreclosing fact-finding). The
 *   claim/metric gap is intentional: this constraint is CLAIMED as a snare
 *   (pure extraction with coercive cover story); the metrics describe
 *   extractive operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.89).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.91).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine: Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4').
narrative_ontology:cs_kernel_codification('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', formalized).
narrative_ontology:cs_authority_grounding('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', extraction).
narrative_ontology:cs_interpretation_layer_present('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4').
narrative_ontology:cs_reading_relation('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', foundational, immunity_as_systematic_extraction).
narrative_ontology:cs_axiom_status(immunity_as_systematic_extraction, holdable).
narrative_ontology:cs_axiom_grounding('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', immunity_as_systematic_extraction, empirically_contingent).
narrative_ontology:cs_axiom('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', secondary, no_legitimate_protective_function).
narrative_ontology:cs_axiom_status(no_legitimate_protective_function, holdable).
narrative_ontology:cs_axiom_grounding('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', no_legitimate_protective_function, empirically_contingent).
narrative_ontology:cs_reference_frame('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', constitutional_accountability_and_remedy).
narrative_ontology:cs_drift_state('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1dbfc2e1-8aa2-4e33-8a79-330bb7747bc4', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, municipal_employers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate with near-complete immunity from civil liability for constitutional violations. They can violate Fourth Amendment (search and seizure), Fifth Amendment (due process), Eighth Amendment (cruel punishment) rights with near-zero personal financial consequence. The doctrine shields them by requiring victims to prove they violated a 'clearly established' right — a moving target where courts retroactively declare rights unestablished. Officers benefit from the doctrine by extracting compliance (fear) from community members who face constitutional violation but have no remedy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, agenda_setter).

% Suffer constitutional violations (illegal searches, excessive force, unlawful detention) with systematically foreclosed remedy paths. Even when the violation is clear, the doctrine requires the survivor to prove the right was 'clearly established' in case law — a standard that allows courts to deny liability by declaring the right newly discovered or fact-pattern-specific. Survivors bear the violation itself (injury, trauma, humiliation, wrongful conviction) and lose the civil damages path that would otherwise provide compensation and deterrence.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_survivors, payer,
    powerless, immediate, trapped, national).

% Retain statutory liability under Section 1983 for custom or policy violations, but individual officers' immunity from suit shifts enforcement pressure entirely to municipalities. Cities and counties pay settlements and judgments directly, facing no mechanism to recover from the shielded officers or incentivize them to change practice. The doctrine decouples individual accountability from institutional cost, removing the primary deterrent that would drive departmental policy change.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_employers, payer,
    organized, generational, constrained, national).

% Are excluded from the core decision-making about the doctrine's scope and application. They argue that qualified immunity contradicts constitutional text and precedent, that it converts constitutional guarantees into unenforceable wishes, and that it preserves officer misconduct through procedural immunity rather than substantive justification. Their framing is outside the immunity framework; they advocate for abolition or radical limitation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates, excluded,
    moderate, generational, constrained, national).

% Adjudicates the doctrine's application through recurring summary judgment decisions. The Court maintains the doctrine through case disposition while systematically denying certiorari on challenges that would require the Court to revisit the doctrine itself. The Court's restraint is structural: overturning qualified immunity would expose decades of prior holdings to potential reopening and would require officers to face unprecedented numbers of trials.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Are bound by the Supreme Court's doctrinal ceiling but operate the 'clearly established law' standard day-to-day through summary judgment dismissals. They face competing institutional pressures: law-and-order dockets prefer quick resolution (summary judgment favors immunity); civil rights dockets resist the doctrine's scope. Most lower courts grant qualified immunity as a default procedural move. Some attempt narrower interpretations of the clearly-established requirement, but are regularly reversed or constrained by circuit precedent.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, lower_courts, payer,
    moderate, biographical, constrained, national).

% Possess statutory authority to limit or abolish qualified immunity (Section 1983 is statutory; Congress writes the rules), but have not exercised it despite decades of advocacy and demonstrable harm. State legislatures face police union opposition and federal-preemption constraints. Congress faces partisan division on policing reform. Their observed inaction vindicates the doctrine by default.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The doctrine does not solve a coordination problem; it creates an enforcement void. No party coordinated around qualified immunity to achieve shared benefit — the doctrine emerged from judicial doctrine-making without statutory grounding and persists through institutional inertia and police-union structural opposition to revision.
% TRANSFER_FUNCTION: Transfers constitutional liability from individual officers (who would otherwise face personal damages and career consequences) to municipalities (institutional defendants) and to violation-survivors (who lose the civil remedy). The transfer mechanism: courts dismiss cases at summary judgment by requiring the plaintiff to prove the right was 'clearly established,' a standard that shifts burden retroactively. Officers extract compliance and authority from powerless communities facing violation with no remedy.
% ABSENT_VOICES: Constitutional violation survivors who have already been filtered out of the courtroom by summary judgment, and civil rights organizations and statutory reform advocates who are outside the judicial process. The doctrine's persistence depends on the Supreme Court's restraint from revisiting it (which silences the question entirely) and on the absence of political pressure from legislative action (which has been systematically absent despite recurring national attention).
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers would face personal civil liability for constitutional violations, municipalities would shift enforcement pressure back to individual officers and departments, civil damages would restore as a deterrent, and constitutional violations would meet the remedy mechanism the Constitution promises. Settlements and verdicts would change police behavior by creating personal professional and financial consequence. The entire accountability structure would reorganize.
% FOUNDING_PROBLEM: The doctrine was created by the Supreme Court in Pierson v. Ray (1967) and Harlow v. Fitzgerald (1982) ostensibly to protect government officials from frivolous lawsuits. The asserted problem: without immunity, officials would be paralyzed by fear of constant litigation, unable to make discretionary decisions.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and police organizations assert the problem remains live. Civil rights organizations, empirical studies, and constitutional law scholars attest that the founding problem is either nonexistent (qualified immunity does not significantly reduce frivolous lawsuits) or solved by existing mechanisms (Rule 12(b)(6) motions, anti-SLAPP statutes, sanctions for frivolous claims, Rule 11). The plaintext of the Constitution and Section 1983 do not mention immunity — its necessity exists only in the doctrinal assertion, not in statutory text or founding-era practice.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.89 at endpoint because the doctrine creates near-absolute immunity: even when an officer violates a constitutional right, the plaintiff must prove the right was 'clearly established' in prior case law, a burden that courts manipulate retroactively to grant immunity. The doctrine thus extracts the right to sue from violation survivors — they bear the constitutional violation and lose the remedy. Suppression is higher (0.91) because the mechanism is structural: alternatives (administrative discipline, state tort law remedies) are weaker than federal Section 1983 claims; state law provides less robust remedies, and internal discipline is controlled by the officer's own institution. Accessibility collapse (0.93) reflects that once the 'clearly established' standard is applied, alternatives to accepting the violation disappear — the plaintiff has nearly no path to remedy. Theater (0.62) increases over the interval because the doctrinal defense of qualified immunity has become more elaborate (multiple Supreme Court rulings expanding the clearly-established standard, law-review articles defending officer immunity) while the actual mechanism — foreclosing liability through summary judgment — remains unchanged. The rise in theater marks the widening gap between the asserted justification (necessary protection) and the measured extraction (systematic impunity). Resistance (0.58) is moderate but lower than in comparable snares because individual victims have no organizational capacity; organizational resistance (civil rights groups, reform movements) exists but is structurally excluded from the doctrinal process. The 44-year measurement series shows monotonic increase in extractiveness, suppression, and theater, with resistance growing organizationally but not reaching critical mass at the individual level where the constraint operates.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (officers) and payer (survivors) seats compute dramatically different types from the same structural data. Officers perceive the constraint as rope or scaffold: it coordinates their safety and decision-making freedom. Survivors perceive it as snare: it forecloses remedy while perpetuating vulnerability. Lower courts and the Supreme Court perceive a coordination function (efficient dispute resolution, protection from endless litigation), which this reading interprets as either false consciousness or deliberate cover — the measured extraction is so asymmetric and the beneficiaries so identifiable that the coordination narrative fails the Boltzmann test. Municipal employers perceive an institutional trap: they must enforce the constraint (defend officers in Section 1983 suits) while bearing its cost (settlements and judgments) without access to the deterrence mechanism (officer personal liability) that would prevent repeated violations. The engine's per-seat computation should reveal that the beneficiary seat perceives coordination while the target seat perceives pure extraction — the divergence is the reading's central empirical claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers occupy the beneficiary pole (d near 1.0 toward beneficiary): they collect the immunity directly, with no personal financial or career consequence from constitutional violations. Constitutional violation survivors occupy the target pole (d near 1.0 toward target): they bear the violation and lose the civil remedy. Municipal employers occupy a middle position (d near 0.6): they retain statutory liability but cannot shift it back to officers, creating a perverse incentive structure where they pay for officer misconduct without the mechanism to deter it. Lower courts (d near 0.4) sit near symmetric between the coordinating role (applying established doctrine) and the extraction-enforcing role (denying plaintiffs remedy). The Supreme Court (d near 0.3) appears as a beneficiary of inaction: by denying certiorari on fundamental challenges, the Court preserves the doctrine without having to defend it. This reading does not account for any protective or coordination benefit that might shift officers or courts toward symmetric positions — all apparent coordination (officer safety, judicial efficiency) is interpreted as cover story under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   Qualified immunity exhibits mandatrophy: the founding problem (officers paralyzed by fear of frivolous litigation) is either nonexistent or solved by existing civil procedure mechanisms (Rule 12(b)(6) motions, anti-SLAPP statutes, Rule 11 sanctions). Yet the doctrine persists through inaction by the Supreme Court (denying certiorari), legislative inaction, and institutional inertia (police unions opposing reform, lower courts applying doctrine as default). The constraint is characterized by high theater (0.62 and rising) because the doctrinal defense has become increasingly elaborate — multiple Supreme Court decisions expanding the clearly-established requirement, law-review articles defending officer immunity, framing immunity as necessary for law-enforcement vigor — while the actual operation (foreclosing remedy through summary judgment) remains unchanged and increasingly disconnected from the stated justification. This is classic Piton-adjacent mandatrophy: the constraint persists not because its justification holds but because no party with power has interest in dismantling it. Officers benefit (impunity), municipalities pay but cannot coordinate on reform (federal issue, statute requires majority action), and individuals have no platform. The constraint should be classified as Piton (atrophied function, maintained theatrically) except that the mechanism is more active and weaponized than typical piton inertia — it is an actively-defended snare wearing a piton's theatrical costume.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_competing_readings_forecast,
    'Which of the three sibling readings — accountability void, constitutional fidelity, or protective scaffold — is structurally true of qualified immunity''s operation?',
    'Empirical study of: (1) whether federal officers face measurably different behavioral constraints under qualified immunity vs. state tort law systems without immunity; (2) whether the ''clearly established'' requirement produces summary judgments at rates inconsistent with protection from frivolous suits (as opposed to protection from substantive accountability); (3) whether officer behavior changes when state legislatures abolish local qualified immunity (pilot data from Colorado, New Mexico, Tennessee civil rights reforms).',
    'If accountability void is correct, the constraint should compute as Snare from the officer seat and all organizational seats, with suppression manifesting as legal foreclosure. If protective scaffold is correct, the constraint should show protective benefit to officers and measurable reduction in frivolous litigation (falsifiable by studying actual litigation rates). If constitutional fidelity is correct, the classification depends on functional extraction — a foundational illegitimacy that is structurally independent of beneficial outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_competing_readings_forecast, empirical, 'Which doctrinal reading''s empirical predictions hold: does qualified immunity actually protect officers from frivolous suits (protective scaffold) or systematically shield them from meritorious suits (accountability void)?').

omega_variable(
    beneficiary_capture_vs_coordination,
    'Do the beneficiaries of qualified immunity — law enforcement officers — collectively organize to defend it (capture), or does the doctrine persist through structural inertia and fragmented payer opposition (piton)?',
    'Analysis of legislative reform efforts: if qualified immunity persists despite repeated bipartisan reform proposals (George Floyd era, post-2020 moment), that suggests payer fragmentation rather than active beneficiary capture. If reform proposals fail due to active police-union opposition (verifiable through legislative testimony and police organization statements), that suggests organized beneficiary capture. If reform fails due to elite indifference and low salience, that suggests piton inertia.',
    'If beneficiary capture: the constraint is Snare with an organized defender (police unions) and fragmented payers (municipalities, individuals). If piton inertia: the constraint is Piton with no concentrated beneficiary but also no concentrated cost-bearer powerful enough to remove it. The classification shifts from Snare (active extraction defended by beneficiaries) to Piton (atrophied defense, persisting through inaction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_coordination, empirical, 'Whether qualified immunity persists as an actively defended benefit to organized officers or as an institutional artifact maintained through inertia and uncoordinated payer opposition.').

omega_variable(
    clearly_established_moving_target_mechanism,
    'Is the ''clearly established law'' standard a genuine procedural screen against frivolous claims (protective scaffold) or a mechanism courts use to deny liability retroactively by declaring rights unestablished after violation (accountability void)?',
    'Doctrinal analysis: (1) empirical study of how courts apply the clearly-established standard — does it correlate with actual frivolousness of claims (trivial damages, lack of factual support) or does it correlate with rights that courts declare unestablished only after the violation is established at summary judgment? (2) Comparative analysis: jurisdictions that abolished qualified immunity (state-level reforms) — do their courts use different standards to screen claims? Do frivolous-claim rates increase, stay stable, or decrease?',
    'If the standard functions as a genuine frivolousness screen, it supports the protective_scaffold reading and suggests suppression is procedurally justified. If the standard functions as a retroactive immunity mechanism divorced from case merits, it confirms the accountability_void reading and marks suppression as extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_moving_target_mechanism, empirical, 'Whether the clearly-established-law standard functions as a frivolousness filter (justifying immunity for procedural reasons) or as a doctrinal immunity shield applied retroactively to meritorious claims (extraction mechanism).').

omega_variable(
    reading_foreclosure_contradiction,
    'Does the accountability void reading''s core claim (qualified immunity operates as pure extraction without legitimate protective function) logically foreclose the protective scaffold reading (immunity enables vigorous law enforcement by protecting officers from litigation burden), or do both readings remain empirically contestable within a single constitutional framework?',
    'Logical analysis: Can the same legal doctrine simultaneously extract impunity from violation survivors AND protect officers from frivolous litigation? If empirical data shows officers receive immunity from both frivolous and meritorious suits (indiscriminately protective), the readings coexist (both partially true). If empirical data shows immunity selectively applied to shield officers from meritorious claims while leaving frivolous-lawsuit doctrines (Rule 12, anti-SLAPP) adequate, then accountability void forecloses protective scaffold — they cannot both be true of the same mechanism.',
    'If readings coexist: qualified immunity is Tangled Rope (genuinely protective AND extractive), and classification depends on seat perspective. If accountability void forecloses protective scaffold: qualified immunity is Snare for all seats except those convinced by the protective narrative (who are mistaken about the mechanism''s function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_contradiction, conceptual, 'Whether the accountability void and protective scaffold readings are logically incompatible (foreclose) or empirically contestable within a single framework (coexist).').

omega_variable(
    suppression_scope_racial_disparity,
    'Does qualified immunity suppress liability equally across all officer populations and violation types, or does suppression concentrate on officers facing minority plaintiffs and communities of color?',
    'Empirical study of summary judgment dispositions controlling for: (1) officer race and plaintiff race; (2) violation type (excessive force, unlawful search, unlawful detention); (3) circuit court (known for differential immunity application rates). Hypothesis: if suppression is symmetric, clearly-established determinations should be independent of plaintiff race and violation context; if suppression concentrates on minority plaintiffs, the doctrine masks race-specific extraction.',
    'If suppression is symmetric and mechanical: qualified immunity is a race-neutral extraction mechanism. If suppression concentrates on minority plaintiffs: the constraint embeds a second extraction layer (race-based targeting) invisible in the doctrine''s text but visible in its application. This would strengthen the Snare classification and reveal an additional victim set (communities of color) with asymmetric vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_scope_racial_disparity, empirical, 'Whether qualified immunity''s suppression mechanism operates uniformly across plaintiff and officer populations or concentrates on officers with minority plaintiffs, revealing a second extraction layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.35).
narrative_ontology:measurement(qual_tr_t1995, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(qual_tr_t2008, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2008, 0.51).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2017, 0.58).
narrative_ontology:measurement(qual_tr_t2023, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2023, 0.62).
narrative_ontology:measurement(qual_tr_t2026, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2026, 0.62).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.72).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(qual_be_t2008, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2008, 0.84).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2017, 0.87).
narrative_ontology:measurement(qual_be_t2023, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2023, 0.89).
narrative_ontology:measurement(qual_be_t2026, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.68).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1995, 0.74).
narrative_ontology:measurement(qual_su_t2008, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2008, 0.82).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2017, 0.88).
narrative_ontology:measurement(qual_su_t2023, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2023, 0.91).
narrative_ontology:measurement(qual_su_t2026, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2026, 0.91).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1982, tn=2026
narrative_ontology:measurement(qual_grid_01, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(class), 1982, 0.61).
narrative_ontology:measurement(qual_grid_02, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(class), 2026, 0.84).
narrative_ontology:measurement(qual_grid_03, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(individual), 1982, 0.68).
narrative_ontology:measurement(qual_grid_04, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(individual), 2026, 0.93).
narrative_ontology:measurement(qual_grid_05, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(organizational), 1982, 0.52).
narrative_ontology:measurement(qual_grid_06, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(organizational), 2026, 0.71).
narrative_ontology:measurement(qual_grid_07, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(structural), 1982, 0.55).
narrative_ontology:measurement(qual_grid_08, qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse(structural), 2026, 0.88).
narrative_ontology:measurement(qual_grid_09, qualified_immunity_doctrine__accountability_void_reading, resistance(class), 1982, 0.42).
narrative_ontology:measurement(qual_grid_10, qualified_immunity_doctrine__accountability_void_reading, resistance(class), 2026, 0.58).
narrative_ontology:measurement(qual_grid_11, qualified_immunity_doctrine__accountability_void_reading, resistance(individual), 1982, 0.31).
narrative_ontology:measurement(qual_grid_12, qualified_immunity_doctrine__accountability_void_reading, resistance(individual), 2026, 0.24).
narrative_ontology:measurement(qual_grid_13, qualified_immunity_doctrine__accountability_void_reading, resistance(organizational), 1982, 0.48).
narrative_ontology:measurement(qual_grid_14, qualified_immunity_doctrine__accountability_void_reading, resistance(organizational), 2026, 0.67).
narrative_ontology:measurement(qual_grid_15, qualified_immunity_doctrine__accountability_void_reading, resistance(structural), 1982, 0.36).
narrative_ontology:measurement(qual_grid_16, qualified_immunity_doctrine__accountability_void_reading, resistance(structural), 2026, 0.61).
narrative_ontology:measurement(qual_grid_17, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(class), 1982, 0.51).
narrative_ontology:measurement(qual_grid_18, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(class), 2026, 0.85).
narrative_ontology:measurement(qual_grid_19, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(individual), 1982, 0.54).
narrative_ontology:measurement(qual_grid_20, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(individual), 2026, 0.92).
narrative_ontology:measurement(qual_grid_21, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(organizational), 1982, 0.48).
narrative_ontology:measurement(qual_grid_22, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(organizational), 2026, 0.76).
narrative_ontology:measurement(qual_grid_23, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(structural), 1982, 0.44).
narrative_ontology:measurement(qual_grid_24, qualified_immunity_doctrine__accountability_void_reading, stakes_inflation(structural), 2026, 0.79).
narrative_ontology:measurement(qual_grid_25, qualified_immunity_doctrine__accountability_void_reading, suppression(class), 1982, 0.67).
narrative_ontology:measurement(qual_grid_26, qualified_immunity_doctrine__accountability_void_reading, suppression(class), 2026, 0.89).
narrative_ontology:measurement(qual_grid_27, qualified_immunity_doctrine__accountability_void_reading, suppression(individual), 1982, 0.71).
narrative_ontology:measurement(qual_grid_28, qualified_immunity_doctrine__accountability_void_reading, suppression(individual), 2026, 0.93).
narrative_ontology:measurement(qual_grid_29, qualified_immunity_doctrine__accountability_void_reading, suppression(organizational), 1982, 0.64).
narrative_ontology:measurement(qual_grid_30, qualified_immunity_doctrine__accountability_void_reading, suppression(organizational), 2026, 0.84).
narrative_ontology:measurement(qual_grid_31, qualified_immunity_doctrine__accountability_void_reading, suppression(structural), 1982, 0.58).
narrative_ontology:measurement(qual_grid_32, qualified_immunity_doctrine__accountability_void_reading, suppression(structural), 2026, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, section_1983_municipal_liability).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_department_reform_incentive_structures).

% DUAL FORMULATION NOTE:
% Qualified immunity kernel has three readings in this corpus: this story (accountability_void_reading) claims the doctrine operates as pure extraction; constitutional_fidelity_reading claims it is foundationally illegitimate on text/precedent grounds; protective_scaffold_reading claims it is a necessary transitory protection. All three readings share the same kernel (the judicially-created doctrine in Harlow v. Fitzgerald) but author different ε values and victim/beneficiary structures. They are not alternative measurements of one constraint — they are different constraints instantiated by different readings. Network edges link them as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, analytical, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
