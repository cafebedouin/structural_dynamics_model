% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity via Positivist Procedure (Law/Morality Distinction)
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   The positivist reading of constitutional authority holds that
 *   constitutional validity derives from formal enactment procedures and
 *   institutional sources (text, enactment history, statutory precedent), not
 *   from moral content or natural law. This reading maintains a sharp
 *   distinction between law and morality: courts determine constitutional
 *   meaning by reference to institutional sources alone; moral philosophy and
 *   justice arguments are declared categorically irrelevant to validity,
 *   though they may inform legislative action. The reading is ONE of three
 *   structurally distinct readings of the contested kernel
 *   'constitutional_text_authority'. It shares with originalism a commitment
 *   to textual constraint but diverges on the grounding of authority
 *   (positivist: institutional procedure; originalist: historical public
 *   understanding, which can slide toward natural law). It explicitly rejects
 *   the living constitutionalist reading, which treats moral evolution as a
 *   source of constitutional meaning. The measurement series tracks the
 *   constraint's extractiveness and enforcement over 40 years: extraction
 *   rises modestly (0.28 → 0.41) as the positivist reading's institutional
 *   dominance expands through legal education and judicial appointment,
 *   leveling off as it reaches institutional saturation. Theater ratio and
 *   suppression remain low because the constraint's enforcement is primarily
 *   through institutional gatekeeping (what counts as valid constitutional
 *   argument in law schools, courts, and legal literature) rather than
 *   through performative activity or coercive suppression — though resistance
 *   from moral philosophers and living constitutionalists persists throughout
 *   the interval.
 *
 * KEY AGENTS:
 *   - institutional_courts: Judges and court systems that adopt the positivist reading as their interpretive standard; they set the agenda for what counts as valid constitutional reasoning
 *   - legal_formalism_practitioners: Scholars, judges, and lawyers invested in the law/morality distinction; they benefit from the reading's institutional dominance
 *   - moral_philosophy_advocates: Philosophers and justice-centered theorists excluded from the constitutional validity conversation; they bear the cost of their arguments being declared categorically irrelevant
 *   - living_constitutionalist_judges: A rival institutional seat holding the opposite reading; excluded by positivism's dominance
 *   - originalist_judges: A structurally related institutional seat; they share positivism's textual fidelity but ground authority differently
 *   - public_citizens: Powerless beneficiaries (predictable law) and payers (constrained rights arguments); they cannot exit the constitutional order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.41).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.28).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity via Positivist Procedure (Law/Morality Distinction)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '8a2935b8-c526-4dde-b815-25c7b2c637bb').
narrative_ontology:cs_kernel_codification('8a2935b8-c526-4dde-b815-25c7b2c637bb', fixed_text).
narrative_ontology:cs_authority_grounding('8a2935b8-c526-4dde-b815-25c7b2c637bb', extraction).
narrative_ontology:cs_interpretation_layer_present('8a2935b8-c526-4dde-b815-25c7b2c637bb').
narrative_ontology:cs_reading_relation('8a2935b8-c526-4dde-b815-25c7b2c637bb', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a2935b8-c526-4dde-b815-25c7b2c637bb', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('8a2935b8-c526-4dde-b815-25c7b2c637bb', foundational, law_morality_dichotomy).
narrative_ontology:cs_axiom_status(law_morality_dichotomy, holdable).
narrative_ontology:cs_axiom_grounding('8a2935b8-c526-4dde-b815-25c7b2c637bb', law_morality_dichotomy, conventional).
narrative_ontology:cs_axiom('8a2935b8-c526-4dde-b815-25c7b2c637bb', secondary, institutional_procedure_legitimacy).
narrative_ontology:cs_axiom_status(institutional_procedure_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8a2935b8-c526-4dde-b815-25c7b2c637bb', institutional_procedure_legitimacy, conventional).
narrative_ontology:cs_reference_frame('8a2935b8-c526-4dde-b815-25c7b2c637bb', formalist_legal_authority).
narrative_ontology:cs_drift_state('8a2935b8-c526-4dde-b815-25c7b2c637bb', contemporary_moral_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a2935b8-c526-4dde-b815-25c7b2c637bb', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, institutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_formalism_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, public_citizens).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_philosophy_advocates).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, public_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution using positivist criteria: institutional sources (text, enactment history, precedent) determine validity; moral philosophy is declared irrelevant to constitutional meaning. Courts that adopt this reading administer the Constitution as a determinate legal artifact, not as a vessel for evolving moral principle. They benefit from the clarity and constraint this framing provides — it narrows judicial discretion and delegates moral judgment to the legislative process.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, institutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Scholars, judges, and practitioners who build careers on the positivist reading. They author treatises on legal formalism, win cases by appeal to textual clarity, and hold faculty positions predicated on the distinction between legal and moral reasoning. The reading's institutional dominance in legal education (at least in Anglo-American law schools) benefits their professional standing.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_formalism_practitioners, beneficiary,
    powerful, biographical, mobile, national).

% Philosophers, legal theorists, and activists who argue that constitutional meaning should be informed by or grounded in moral principles (rights theory, dignity, justice). The positivist reading denies them a seat at the constitutional interpretation table — their moral arguments are declared categorically irrelevant to validity, regardless of strength or consensus. They must work through legislative amendment or persuade courts to adopt a different reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_philosophy_advocates, payer,
    moderate, biographical, constrained, national).

% Judges and legal scholars who adopt the living constitutionalist reading (sibling constraint). The positivist reading directly challenges their interpretive authority by declaring that moral evolution and contemporary values are not sources of constitutional meaning. They are not prohibited from adopting the living constitutionalist frame, but the positivist frame — when institutionally dominant — marginalizes their reasoning as subjective or extra-legal.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, constrained, national).

% Judges and scholars who adopt originalism. They share the positivist reading's commitment to textual constraint and rejection of moral philosophy as a direct source of meaning. However, originalism grounds constitutional meaning in historical public understanding at ratification, a different institutional source than the positivist reading's emphasis on formal enactment procedure and contemporary institutional authority. Where originalism can slide toward natural-law grounding (the Framers' moral intentions), positivism maintains strict proceduralism.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, originalist_judges, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, originalist_judges, excluded).

% The positivist reading delegates moral judgment to the legislature. If courts adopt positivism and declare moral arguments irrelevant to constitutional validity, legislative bodies become the formal site where moral principles inform law. This is presented as a separation of powers virtue but can also concentrate moral authority in majoritarian bodies with weak countermajoritarian protection.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% Citizens whose constitutional rights and protections hinge on how courts interpret the Constitution. The positivist reading constrains judicial discretion (potential benefit: predictability, constraint on judicial overreach) but also forecloses rights arguments grounded in moral principles that do not appear in institutional legal sources (potential cost: rights claims outside the positive legal canon lack standing). Their exit is essentially nil; they are governed by whatever constitutional reading becomes institutionally dominant.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, public_citizens, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, public_citizens, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, institutional_courts).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables courts with sharply different moral views to coordinate on constitutional interpretation using a shared, institutional criterion (text, enactment history, precedent, formal procedure) that does not depend on contested moral agreement. Solves the problem: how can judges with opposing moral philosophies apply the same Constitution to the same cases and reach coordinated legal outcomes? Answer: by referring only to institutional sources and excluding moral philosophy from the validity standard.
% TRANSFER_FUNCTION: Transfers epistemic authority from moral philosophers and justice theorists to institutional legal actors. Moral arguments that do not appear in institutional legal sources (constitutional text, enactment history, precedent) are removed from the legitimate constitutional reasoning set. The authority transferred is the standing to contribute directly to constitutional meaning via moral or philosophical reasoning.
% ABSENT_VOICES: Moral philosophers, natural lawyers, virtue ethicists, and justice-centered theorists are structurally excluded from constitutional validity conversations (though they can petition legislatures). They would argue that law cannot be separated from morality — that constitutional interpretation IS moral reasoning — and that the positivist boundary is incoherent. Their exclusion is enforced through institutional gatekeeping: law school curricula emphasize institutional sources and positivist methodology; bar exams test formalist legal knowledge; judges are appointed from the formalist legal academy. The absence of these voices is institutionally maintained, not accidental.
% DISAPPEARANCE_RATIONALE: If the positivist reading's institutional dominance disappeared and courts adopted living constitutionalism or natural law as interpretive standards, constitutional meaning on core issues (abortion, equality, liberty, dignity) would shift because moral reasoning operates under different criteria than institutional sources alone. A constitution interpreted through moral philosophy would generate different outcomes on contested cases. The judicial consensus that currently relies on positivist criteria would dissolve into competing moral frameworks. Legislation would need to change to accommodate the new constitutional landscape. The stable coordination the reading provides would cease.
% FOUNDING_PROBLEM: In the early 20th century, legal realists demonstrated that judges' moral and political views influenced judicial outcomes, challenging formalism's claim that law was determinate. How could law be stable and knowable if judges' personal philosophies determined outcomes? Positivism proposed an answer: separate law from morality through institutional sources (text, procedure, precedent). Only by excluding moral philosophy can judges coordinate on stable, predictable law.
% FOUNDING_PROBLEM_CORROBORATION: Positivist judges and formalist scholars attest the problem is still live, citing persistent disagreement on constitutional meaning as evidence that institutional criteria are necessary to constrain discretion. Natural lawyers and living constitutionalists attest the founding problem is misconceived — the problem assumes law can be separated from morality, which they deny. Legal realist scholars and critics of formalism provide external corroboration that the founding problem arose from realist challenges and that positivism was a response to those challenges, not a discovery of law's true nature. Contemporary jurisprudence (Dworkin, Fish, Kennedy, Balkin, and others) provides testimony that the law/morality distinction is philosophically contested and institutionally unstable.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.41 at interval end) is moderate because the positivist reading does solve a genuine coordination problem — it provides a shared criterion for constitutional interpretation that does not depend on contested moral agreement. This is a real benefit (rope side). However, extractiveness rises above the coordination baseline because the reading also excludes entire classes of argument (moral philosophy, justice theory) that do not appear in institutional sources, generating asymmetric constraint for agents whose constitutional claims rest on moral reasoning. Suppression is low (0.28) because enforcement happens through institutional gatekeeping (who gets hired as a law professor, whose arguments appear in treatises and court opinions) rather than through coercive force or internalized acceptance of moral inferiority. Theater ratio is very low (0.12) because the constraint's primary function (coordinate on institutional sources) remains functionally active; the small theater component reflects the performative maintenance required whenever the law/morality distinction itself must be defended against challenges that it is incoherent. Accessibility collapse is moderately high (0.72) because once the positivist frame is institutionalized, moral philosophy arguments literally do not count as valid constitutional moves in judicial contexts — alternatives (amendments, legislative morality) collapse as direct constitutional channels. Resistance is moderate-to-high (0.58) because moral philosophers, living constitutionalists, and realist legal scholars mount sustained academic and judicial challenge to the distinction. The measurement trajectory shows the constraint's extraction rising as the positivist reading's institutional grip tightens from 1980 to 2020 (observable in legal-education curricular shifts and judicial appointment patterns), then leveling off as dominance saturation is reached — further enforcement gains are marginal. The shared time grid (40-year interval, 6 measurement points per metric) enables temporal analysis of how the reading's institutional dominance accumulates and where it stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (institutional courts) and the payer seat (moral philosophy advocates) compute radically different constraint types. From the court's position, the positivist reading is genuine coordination — it solves the problem of interpretation disagreement by reference to a shared institutional criterion, and courts see themselves as maintaining law's stability and determinacy. From the moral philosopher's position, the same structure operates as enforced exclusion — their discipline's authority to contribute to constitutional meaning is declared irrelevant by fiat, not by argument. The payer seat experiences this as extraction of epistemic authority disguised as clarification. The engine will compute these seats' classifications differently because their power atoms, exit options, and structural relationships to the constraint diverge substantially: courts are institutional with analytical exit options (they can adopt a different reading), while moral philosophers are powerful but professionally constrained (their careers depend on having some institutional purchase on constitutional meaning). The measured extraction (0.41) reflects the court's reading; from the moral philosopher's seat, effective extraction would be higher because the suppression of alternatives is more complete.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional courts are beneficiaries (d near 0.0–0.2): they collect coordination clarity, constrain their own discretion in a way that increases public confidence in law's stability, and gain authority over the interpretation process by controlling which sources count as valid. Legal formalism practitioners benefit similarly (d near 0.1–0.3): their careers are built on the law/morality distinction. Moral philosophy advocates are targets (d near 0.7–0.85): their arguments are excluded from constitutional validity by categorical rule, not by argument; their discipline loses authority in constitutional contexts; their ability to persuade on moral grounds is declared irrelevant. Public citizens are near-symmetric (d near 0.45–0.55): they benefit from predictable constitutional law and from the constraint's role in preventing judicial overreach, but they pay through constrained rights arguments that cannot appeal to moral principles outside institutional sources. Living constitutionalist judges occupy an interesting position (d near 0.55–0.65): they are institutional actors but excluded by the positivist reading's dominance; they experience the constraint as limiting their legitimate hermeneutical authority. Originalist judges are partial beneficiaries (d near 0.3–0.4): they share positivism's textualism and institutional grounding but their natural-law-adjacent moorings make them slightly less aligned with strict positivism. The directionality overrides are unnecessary here because the structural relationships flow naturally from beneficiary/victim declarations and exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading initially solved a live problem — the legal realist challenge to formalism — by proposing a criterion (institutional sources) for determinate law that does not depend on contested moral agreement. The founding problem (how can law be stable across judges with different moral views?) was live and urgent in the early 20th century. By mid-century, the solution had solidified into institutional doctrine: law schools taught the law/morality distinction, judges adopted it as their standard, and the coordinate solution became self-reinforcing. By 2020, the founding problem's status is contested: positivists argue the problem remains live (judicial disagreement persists; institutional criteria are still needed to constrain discretion), while critics argue the founding problem is dead (the problem was misconceived; the law/morality distinction is philosophically indefensible). The disappearance verdict (world_rearranges) reflects that the reading's institutional dominance shapes constitutional jurisprudence substantively — if the reading disappeared and courts adopted moral philosophy as a source of constitutional meaning, constitutional law would shift. This is not a mandatrophy case because the founding problem remains contested and the coordinate solution, while aged, remains functionally active in legal institutions. The reading has not atrophied into pure theater; it still performs the function of coordinate-on-institutional-sources. However, the rising theater ratio (0.04 → 0.12) and rising suppression requirement (0.18 → 0.28) suggest that defending the law/morality distinction itself requires increasing performative effort — the distinction is increasingly questioned and the constraint's maintenance requires more active institutional gatekeeping. This is pre-mandatrophy drift, not yet mandatrophy: the reading is still solving a problem, but the problem's reality is increasingly contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    law_morality_distinction_coherence,
    'Is the law/morality distinction coherent, or is it a philosophical false boundary that constitutional interpretation necessarily violates?',
    'Philosophical analysis of whether law can operate without moral content or justification; empirical analysis of whether courts that claim to follow positivism actually exclude moral reasoning in practice.',
    'If the distinction is incoherent, the positivist reading''s claim to exclude morality is false — courts would be performing positivism while actually practicing moral reasoning. This would reclassify the constraint from a coordination mechanism to a theatrical suppression (high mandatrophy). If the distinction is coherent and maintainable, the reading''s coordination function is real and the suppression of moral philosophy as a direct source is a structural feature, not a performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(law_morality_distinction_coherence, conceptual, 'Whether the law/morality distinction is a sustainable boundary or a false dichotomy').

omega_variable(
    institutional_sources_determinacy,
    'Can institutional sources (text, enactment history, precedent) actually determine constitutional meaning without recourse to moral or philosophical principles?',
    'Analysis of actual judicial reasoning in constitutional cases: do courts claiming positivist grounds avoid all moral or philosophical premises? What happens when institutional sources conflict or are ambiguous?',
    'If institutional sources alone cannot determine meaning, the positivist reading fails to solve its founding problem (determinate law without contested morality) — extraction becomes pure, not rope. If institutional sources can determine meaning, the reading''s coordination solution is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_sources_determinacy, empirical, 'Whether institutional legal sources are sufficient to determine constitutional meaning').

omega_variable(
    natural_law_creep_in_originalism,
    'Does originalism, which shares positivism''s textual constraint, nevertheless slide back toward natural law through the Framers'' moral intentions?',
    'Comparative analysis of originalist judicial opinions: do they claim to follow historical public understanding alone, or do they appeal to natural law, rights, or justice principles in interpreting that understanding?',
    'If originalism is structurally dependent on natural law moorings (grounding in the Framers'' moral vision), then originalism and positivism are structurally opposed at their core, and their surface agreement on textualism masks a deeper divide. This would tighten the ''influences'' relationship between positivism and originalism — positivism would actively exclude the natural-law grounding that originalism implicitly relies on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_creep_in_originalism, empirical, 'Whether originalism avoids or depends on natural law moorings').

omega_variable(
    moral_argument_venue_shift,
    'When courts adopt the positivist reading and exclude moral philosophy from constitutional validity, does moral reasoning simply migrate to the legislature, or does it disappear from law entirely?',
    'Empirical analysis of legislative debates and statutory law: are they informed by the same moral philosophy that courts exclude? Do moral arguments carry equal weight in legislative vs. judicial contexts?',
    'If moral reasoning migrates to the legislature without loss, the positivist reading''s suppression of moral philosophy is a venue shift, not an elimination. The constraint then redistributes interpretive authority from courts (which lose moral argument standing) to legislatures (which gain it as the legitimate moral-reasoning site). If moral reasoning is weakened in the legislative venue, the suppression is more complete — moral philosophy loses influence over law regardless of venue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_argument_venue_shift, empirical, 'Whether excluding moral philosophy from courts eliminates it from law or redirects it to the legislature').

omega_variable(
    kernel_reading_contest_stability,
    'Which reading (originalist, living constitutionalist, positivist) will dominate constitutional jurisprudence over the next 40 years?',
    'Track judicial appointment patterns, law school curriculum shifts, and appellate opinion trends to see which reading''s institutional presence grows, stabilizes, or declines.',
    'If the positivist reading''s dominance weakens, the measurement trajectory''s plateau (t=24–40) would be falsified — extraction would rise again as the reading is contested and institutional gatekeeping becomes more visible. If positivism''s dominance strengthens, the plateau would be confirmed. The contest outcome determines whether this reading remains a live constitutional constraint or drifts toward mandatrophy as courts defect to rival readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_stability, empirical, 'Which reading will institutionally dominate constitutional interpretation in the future').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__positivist_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__positivist_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__positivist_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__positivist_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__positivist_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__positivist_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__positivist_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__positivist_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__positivist_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__positivist_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__positivist_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__positivist_reading, suppression_requirement, 32, 0.28).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The 'constitutional_text_authority' kernel is instantiated by three structurally distinct constraint stories: (1) originalist_reading = constitutional meaning fixed at ratification by historical public understanding; (2) living_constitutionalist_reading = constitutional meaning evolves with social values and contemporary moral principles; (3) positivist_reading (this story) = constitutional validity determined by formal enactment procedures and institutional sources, law/morality distinction maintained. Each reading has a different epsilon (ε), different beneficiary/victim structure, and different institutional mechanism. They coexist as live positions held by different judicial and scholarly factions. All three share the kernel text (the Constitution) and the founding problem (how to interpret the Constitution without judges' personal moral views determining the outcome), but they propose radically different answers. The three stories are linked by network.affects_constraints to show the kernel contest: each reading's institutional dominance or decline affects the other readings' legitimacy and institutional standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
