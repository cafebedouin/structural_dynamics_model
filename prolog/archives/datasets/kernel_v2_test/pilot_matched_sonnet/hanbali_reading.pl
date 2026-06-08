% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Methodological Constraint on Jurisprudential Reasoning
 *   domain: islamic_jurisprudence/legal_theory/usul_al_fiqh
 *
 * SUMMARY:
 *   The Hanbali methodological constraint restricts jurisprudential reasoning
 *   to text literalism and minimal analogical reasoning (qiyas), preferring
 *   even weak hadith over analogical extension. This reading of usul al-fiqh
 *   (Islamic legal theory) emerged in 9th-century Baghdad as Ahmad ibn
 *   Hanbal's response to Mu'tazilite rationalism and the mihna (inquisition).
 *   The constraint coordinates traditionalist scholarship by establishing
 *   clear hierarchies of legal sources (Qur'an > Sunnah > weak hadith >
 *   qiyas) while simultaneously extracting from rationalist interpreters
 *   whose training and intellectual commitments emphasize analogical
 *   reasoning. The method is one of four major Sunni approaches (alongside
 *   Hanafi, Maliki, and Shafi'i readings), each offering different balances
 *   between textual authority and rational extension. The Hanbali reading's
 *   extractiveness has increased over the interval (0.35 → 0.48) as
 *   institutional consolidation has hardened the methodological boundaries
 *   and reduced space for internal pluralism. Theater ratio remains
 *   relatively low (0.35) because the constraint's function is genuine: it
 *   does coordinate legal reasoning and does preserve prophetic precedent,
 *   even as it extracts from excluded interpretive approaches. Suppression
 *   has intensified (0.50 → 0.62) as the method has become
 *   identity-constitutive for scholars trained within Hanbali institutions.
 *
 * KEY AGENTS:
 *   - Traditionalist Scholars: Primary beneficiaries (institutional/arbitrage) — the constraint validates hadith expertise as supreme jurisprudential skill, concentrating authority in traditionalist networks
 *   - Hadith Transmission Networks: Institutional beneficiaries (institutional/constrained) — elevated status of hadith scholarship but burdened by authentication labor requirements
 *   - Textualist Jurists: Beneficiaries (institutional/arbitrage) — the method privileges their interpretive approach over rationalist competitors
 *   - Rationalist Interpreters: Primary victims (powerless/identity_locked) — structurally excluded from exercising analogical reasoning; exit requires abandoning professional identity within the madhhab
 *   - Analogical Reasoning Practitioners: Victims (moderate/constrained) — methodological tools foreclosed; can exit to other schools at career cost
 *   - Novel Case Adjudicators: Mixed position (moderate/constrained) — benefit from decisional framework but constrained when facing unprecedented questions where analogy would be more coherent than weak hadith
 *   - Contemporary Reformist Coalition: Organized agents (organized/mobile) — building alternative frameworks (maqasid-centered jurisprudence) with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.48).
domain_priors:suppression_score(hanbali_reading, 0.62).
domain_priors:theater_ratio(hanbali_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hanbali_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Methodological Constraint on Jurisprudential Reasoning").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/usul_al_fiqh").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, '1101bc8e-f849-4c97-a745-1003ab351d43').
narrative_ontology:cs_kernel_codification('1101bc8e-f849-4c97-a745-1003ab351d43', fixed_text).
narrative_ontology:cs_authority_grounding('1101bc8e-f849-4c97-a745-1003ab351d43', lineage).
narrative_ontology:cs_interpretation_layer_present('1101bc8e-f849-4c97-a745-1003ab351d43').
narrative_ontology:cs_reading_relation('1101bc8e-f849-4c97-a745-1003ab351d43', hanbali_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1101bc8e-f849-4c97-a745-1003ab351d43', hanbali_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1101bc8e-f849-4c97-a745-1003ab351d43', hanbali_reading__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('1101bc8e-f849-4c97-a745-1003ab351d43', foundational, textual_precedent_supremacy).
narrative_ontology:cs_axiom_status(textual_precedent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1101bc8e-f849-4c97-a745-1003ab351d43', textual_precedent_supremacy, deontological).
narrative_ontology:cs_axiom('1101bc8e-f849-4c97-a745-1003ab351d43', foundational, analogical_reasoning_minimization).
narrative_ontology:cs_axiom_status(analogical_reasoning_minimization, holdable).
narrative_ontology:cs_axiom_grounding('1101bc8e-f849-4c97-a745-1003ab351d43', analogical_reasoning_minimization, deontological).
narrative_ontology:cs_reference_frame('1101bc8e-f849-4c97-a745-1003ab351d43', prophetic_precedent_sufficiency).
narrative_ontology:cs_drift_state('1101bc8e-f849-4c97-a745-1003ab351d43', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1101bc8e-f849-4c97-a745-1003ab351d43', '').
narrative_ontology:cs_kernel_id(hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, traditionalist_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, hadith_transmission_networks).
narrative_ontology:constraint_beneficiary(hanbali_reading, textualist_jurists).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_interpreters).
narrative_ontology:constraint_victim(hanbali_reading, analogical_reasoning_practitioners).
narrative_ontology:constraint_victim(hanbali_reading, novel_case_adjudicators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hanbali_reading, novel_case_adjudicators).
narrative_ontology:constraint_victim(hanbali_reading, hadith_transmission_networks).
narrative_ontology:constraint_vindicates(hanbali_reading, prophetic_precedent_supremacy).
narrative_ontology:constraint_vindicates(hanbali_reading, textual_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose expertise centers on hadith transmission and textual authentication. The Hanbali method validates their skill set as the supreme jurisprudential qualification, concentrating scholarly authority and institutional resources in traditionalist networks. They can engage with other madhhabs without losing legitimacy within Hanbali circles.
narrative_ontology:constraint_stakeholder(hanbali_reading, traditionalist_scholars, beneficiary,
    institutional, generational, arbitrage, global).

% Institutional networks specializing in hadith collection, authentication, and transmission. Gain elevated status from the method's preference for weak hadith over qiyas, but bear the burden of maintaining authentication standards rigorous enough to justify that preference. Exit to other scholarly networks possible but costly.
narrative_ontology:constraint_stakeholder(hanbali_reading, hadith_transmission_networks, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hanbali_reading, hadith_transmission_networks, payer).

% Jurists trained in literal textual interpretation. The method privileges their interpretive approach over rationalist competitors, providing career advantages and institutional authority. Can move between textualist and rationalist contexts with minimal cost.
narrative_ontology:constraint_stakeholder(hanbali_reading, textualist_jurists, beneficiary,
    institutional, biographical, arbitrage, national).

% Scholars trained in analogical reasoning and rational extension of legal principles. The Hanbali method forecloses the interpretive tools their training emphasized. Career advancement and scholarly legitimacy within Hanbali institutions require conforming to textualist method despite intellectual commitment to rationalist approaches. Exit would require abandoning professional identity within the madhhab.
narrative_ontology:constraint_stakeholder(hanbali_reading, rationalist_interpreters, payer,
    powerless, biographical, identity_locked, regional).

% Jurists whose training included substantial qiyas methodology. The Hanbali restriction forecloses their primary interpretive tool. Can exit to Hanafi or Shafi'i institutions where qiyas is permitted, but at career cost (loss of institutional position, scholarly network, accumulated reputation within Hanbali circles).
narrative_ontology:constraint_stakeholder(hanbali_reading, analogical_reasoning_practitioners, payer,
    moderate, biographical, constrained, national).

% Jurists facing unprecedented legal questions where existing hadith (even weak hadith) provides limited guidance. Benefit from the method's clear source hierarchy (provides decisional framework) but constrained when analogical reasoning would produce more coherent outcomes than available weak hadith. Can exit to other madhhabs at career cost.
narrative_ontology:constraint_stakeholder(hanbali_reading, novel_case_adjudicators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hanbali_reading, novel_case_adjudicators, beneficiary).

% Organized movements (Islamic modernism, maqasid-centered jurisprudence) building alternative frameworks that preserve traditionalist concerns while reintegrating analogical tools. See the Hanbali restriction as a temporary historical formation responding to 9th-century rationalist excess. Mobile across institutional contexts; not bound by madhhab loyalty.
narrative_ontology:constraint_stakeholder(hanbali_reading, contemporary_reformist_coalition, observer,
    organized, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Hanbali method coordinates legal reasoning by establishing a clear hierarchy of sources (Qur'an > Sunnah > weak hadith > qiyas) and preventing arbitrary legal innovation through text literalism. It solves the collective action problem of maintaining jurisprudential consistency across a distributed scholarly network without centralized authority.
% TRANSFER_FUNCTION: The method transfers scholarly authority and institutional resources from rationalist interpreters (whose analogical reasoning skills are devalued) to traditionalist scholars (whose hadith expertise is elevated). Career advancement, funding, and legitimacy flow toward textualist approaches and away from rationalist ones.
% ABSENT_VOICES: Rationalist scholars who left or were excluded from Hanbali institutions during the method's consolidation (9th-11th centuries). Contemporary jurists trained in comparative law who see the qiyas restriction as unnecessarily constraining. Laypersons facing novel legal questions where weak hadith provides inadequate guidance. These voices would contest the method's necessity and argue for broader analogical reasoning, but they are structurally excluded from Hanbali institutional decision-making.
% DISAPPEARANCE_RATIONALE: If the Hanbali methodological constraint disappeared, jurisprudential practice within Hanbali institutions would rearrange substantially. Rationalist interpreters would resume analogical reasoning; novel case adjudicators would prioritize coherent qiyas over weak hadith when facing unprecedented questions; the career advantage of hadith expertise over rational extension would diminish. The constraint organizes real institutional arrangements (hiring, promotion, scholarly legitimacy, curriculum design) — its disappearance would redistribute authority and resources.
% FOUNDING_PROBLEM: The Hanbali method was built to solve the problem of rationalist excess in 9th-century Islamic jurisprudence. The Mu'tazilite school's emphasis on rational theology and analogical reasoning was seen by traditionalists as threatening to displace prophetic precedent and textual authority. The mihna (inquisition, 833-851 CE) where rationalist doctrine was enforced by state power crystallized traditionalist resistance. Ahmad ibn Hanbal's textualist method was a response: preserve prophetic precedent by restricting rational extension and preferring even weak hadith over analogical reasoning.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist Hanbali scholars attest that rationalist excess remains a live threat — they point to contemporary reformist movements (Islamic modernism, maqasid jurisprudence) as evidence that the founding problem persists. Reformist scholars contest this: they argue that the 9th-century rationalist threat was specific to Mu'tazilite theology and state enforcement, neither of which exists in contemporary form. The Mu'tazilite school itself is extinct; no contemporary movement advocates state-enforced rationalist doctrine. Corroborating sources outside the beneficiary set (comparative legal historians, non-Hanbali jurists) generally assess the founding problem as historically resolved: the specific rationalist excess the method was built to counter no longer exists, though the method persists through institutional inertia and identity lock.
narrative_ontology:disappearance_verdict(hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(hanbali_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONALIST INTERPRETER (SNARE) — Identity-locked within the Hanbali tradition but structurally excluded from exercising analogical reasoning (qiyas). The constraint forecloses the interpretive tools their training emphasized. Exit would require abandoning professional identity within the madhhab. High extraction: career advancement and scholarly legitimacy depend on conforming to textualist method despite intellectual commitment to rationalist tools.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: NOVEL CASE ADJUDICATOR (TANGLED ROPE) — Constrained by methodological restrictions when facing unprecedented legal questions. Benefits from the coordination function (clear hierarchy of sources provides decisional framework) but bears extraction cost (weak hadith must be preferred over analogical reasoning even when analogy would produce more coherent outcomes). Can exit to another madhhab at career cost. Mixed experience: the method both enables and constrains adjudication.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRADITIONALIST SCHOLAR NETWORK (ROPE) — Primary beneficiary. The constraint validates hadith transmission expertise as the supreme jurisprudential skill, concentrating scholarly authority in traditionalist networks. Experiences the method as pure coordination: it solves the genuine problem of preventing arbitrary legal innovation while preserving prophetic precedent. Arbitrage exit available (can engage with other schools) but no incentive to leave. Low effective extraction — the constraint subsidizes this position.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTEMPORARY REFORMIST COALITION (SCAFFOLD) — Organized movements (Islamic modernism, maqasid-centered jurisprudence) see the textualist constraint as a temporary historical formation responding to 9th-century rationalist excess. The sunset logic: as hermeneutical sophistication matures and the original polemical context fades, the methodological restriction loses its coordinating function. Reform movements are building alternative frameworks (maqasid al-sharia, maslaha-based reasoning) that preserve traditionalist concerns while reintegrating analogical tools. Estimated sunset: generational transition as reformist institutions gain legitimacy.
constraint_indexing:constraint_classification(hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HADITH TRANSMISSION NETWORK (TANGLED ROPE) — Institutional beneficiary but also constrained by the method's demands. Benefits from elevated status of hadith expertise but bears the extraction cost of maintaining transmission standards rigorous enough to justify preferring weak hadith over qiyas. The constraint coordinates hadith scholarship but extracts through the requirement for continuous authentication labor. Mixed experience: the method both empowers and burdens hadith specialists.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, the Hanbali method solves a genuine coordination problem (preventing arbitrary legal innovation, preserving prophetic precedent) while simultaneously extracting from rationalist interpreters and novel case adjudicators. The constraint is not a natural law — alternative methodologies (Hanafi qiyas-permissive approach, Maliki istislah, Shafi'i structured analogy) demonstrate that the textualist restriction is one contingent solution among several. The analytical classification is tangled_rope: real coordination function with asymmetric extraction embedded in the same structure.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from rationalist interpreters and novel case adjudicators by foreclosing analogical reasoning tools, concentrating jurisprudential authority in traditionalist networks with hadith expertise. The extraction is not maximal because the method does solve a genuine coordination problem (preventing arbitrary legal innovation) and does preserve prophetic precedent. The value reflects real asymmetric cost: rationalist scholars bear career penalties and intellectual constraint, while traditionalists collect legitimacy rents. Suppression (0.62): Moderate-high. Significant barriers to exercising analogical reasoning include institutional enforcement (career penalties for qiyas-permissive rulings), identity lock (scholars trained in Hanbali method internalize textualist framing), and legitimacy costs (rationalist interpretation is framed as bid'ah/innovation). Suppression is not total — exit to other madhhabs is possible, and internal Hanbali debates about qiyas boundaries do occur — but the barriers are substantial. Theater ratio (0.35): Low-moderate. The constraint's coordination function is genuine: it does establish clear source hierarchies, does preserve prophetic precedent, and does prevent some forms of arbitrary innovation. The theater component reflects the gap between the method's claimed necessity (framed as divine intent) and its contingency (demonstrated by functioning alternative methods in sibling schools). The ratio has increased modestly over the interval as institutional consolidation has added performative elements (ritualized citation of weak hadith even when stronger analogical reasoning is available).
 *
 * PERSPECTIVAL GAP:
 *   The Hanbali methodological constraint demonstrates indexical classification across power and exit dimensions. Traditionalist scholars with arbitrage exit see pure coordination (Rope) — the method solves the problem of preserving prophetic precedent and preventing rationalist excess. Novel case adjudicators with constrained exit see mixed coordination and extraction (Tangled Rope) — the method both enables decisional frameworks and forecloses coherent analogical solutions. Rationalist interpreters with identity-locked exit see pure extraction (Snare) — the constraint forecloses their interpretive tools while offering no genuine coordination benefit from their perspective. The reformist coalition with mobile exit sees a temporary problem with a sunset (Scaffold) — the method's historical function is acknowledged but its continued necessity is denied. The analytical observer sees tangled_rope at the civilizational scale: genuine coordination function (source hierarchy, precedent preservation) with embedded asymmetric extraction (rationalist exclusion, analogical reasoning suppression). The perspectival gap is not a measurement error — it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Traditionalist scholars are declared beneficiaries with arbitrage exit — they collect legitimacy rents from the constraint and can engage with other schools without cost, producing low d and negative effective extraction (the constraint subsidizes them). Rationalist interpreters are declared victims with identity-locked exit — they bear career penalties and intellectual constraint, and exit would require abandoning professional identity, producing high d and high effective extraction. Novel case adjudicators are declared victims with constrained exit — they face methodological restrictions but can exit to other madhhabs at career cost, producing moderate d and moderate extraction. Hadith transmission networks are declared beneficiaries but with constrained exit — they gain status but bear authentication labor burdens, producing low-moderate d. The reformist coalition has mobile exit and sees the constraint as temporary, producing low d. The analytical observer has analytical exit and sees the structure from outside any single tradition, producing the baseline d for tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali methodological constraint resolves the mandatrophy by demonstrating that tangled_rope classification captures constraints with genuine coordination functions that simultaneously extract asymmetrically. The method does coordinate legal reasoning (establishes source hierarchies, preserves prophetic precedent, prevents some arbitrary innovation) AND does extract from rationalist interpreters (forecloses analogical tools, concentrates authority in traditionalist networks). The coordination is not a cover story — the method genuinely solves collective action problems in jurisprudential reasoning. The extraction is not incidental — the method structurally privileges one interpretive approach over another, producing identifiable victims. The tangled_rope classification prevents both false negatives (missing the extraction by focusing only on coordination) and false positives (missing the coordination by focusing only on extraction). The constraint is neither pure rope (coordination without extraction) nor pure snare (extraction without coordination) — it is the hybrid the tangled_rope category was designed to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Hanbali textualist method one reading of a contested kernel (usul al-fiqh methodology), or is it the uniquely correct interpretation of divine intent regarding jurisprudential reasoning?',
    'Historical analysis of intra-Hanbali debates about qiyas restriction; examination of whether Hanbali jurists themselves treated the method as interpretive choice or discovered necessity; comparison with sibling readings (Hanafi, Maliki, Shafi''i) to assess whether the methodological divergence is framed as difference in ijtihad or difference in correctness.',
    'If one reading among several: tangled_rope classification confirmed — the constraint coordinates within a tradition while extracting from excluded interpretive approaches. If uniquely correct: mountain classification from the traditionalist perspective — but this would require demonstrating that alternative methods are not merely different but logically impossible, which the existence of functioning sibling schools contradicts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether Hanbali method is one reading of contested usul al-fiqh kernel or uniquely correct interpretation').

omega_variable(
    weak_hadith_threshold,
    'What threshold of hadith weakness triggers the preference for qiyas over textual precedent? Does ''weak hadith preferred over qiyas'' apply to all non-fabricated hadith, or only to hadith meeting minimum transmission standards?',
    'Analysis of Hanbali jurisprudential rulings where weak hadith and analogical reasoning conflict; identification of cases where qiyas was permitted despite available hadith; examination of Ibn Hanbal''s own practice in the Musnad and later Hanbali codifications.',
    'If threshold is low (all non-fabricated hadith): extraction on rationalist interpreters is severe — almost no space for analogical reasoning. If threshold is moderate (only hadith meeting minimum standards): extraction is lower — qiyas remains available for genuinely unprecedented cases. This affects the extractiveness metric and the tangled_rope vs snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_threshold, empirical, 'Threshold of hadith weakness that triggers qiyas preference').

omega_variable(
    analogical_reasoning_suppression_mechanism,
    'Is the suppression of qiyas structural (institutional barriers, career penalties for rationalist interpretation) or internalized (scholars trained in Hanbali method cannot see analogical reasoning as legitimate even when structurally available)?',
    'Post-exit trajectory analysis: scholars who leave Hanbali institutions for rationalist-permissive contexts — do they resume analogical reasoning immediately (structural suppression) or continue to avoid it (internalized suppression)? Examination of Hanbali scholars'' private writings vs public rulings for evidence of self-censorship vs genuine conviction.',
    'If structural: suppression is reversible through institutional change; reformist movements can succeed by changing incentive structures. If internalized: suppression persists after institutional barriers are removed; the constraint has colonized the interpretive imagination itself. This affects the identity_locked vs constrained exit classification and the suppression metric''s interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogical_reasoning_suppression_mechanism, empirical, 'Whether qiyas suppression is structural or internalized').

omega_variable(
    sibling_reading_coexistence,
    'Do the four Sunni madhhab readings (Hanbali, Hanafi, Maliki, Shafi''i) genuinely coexist as equally valid interpretive frameworks, or does each reading''s internal logic foreclose the others within its own framework?',
    'Analysis of cross-madhhab polemics: do Hanbali jurists treat Hanafi qiyas-permissive method as wrong-but-tolerable (coexistence) or as logically incoherent given shared premises (foreclosure)? Examination of whether a single jurist could coherently hold Hanbali textualism and Hanafi rationalism simultaneously, or whether adopting one commits the jurist to rejecting the other.',
    'If coexistence: the reading_relations should be coexists_with for all siblings — the madhhab system is a pluralistic framework where multiple methods are live options. If foreclosure: at least some reading_relations should be forecloses — each method''s foundational axioms rule out the others within a single coherent framework. This affects the cs_structure.reading_relations declarations and the conceptual vs preference omega classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling madhhab readings coexist or foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_theater_founding, hanbali_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanbali_theater_consolidation, hanbali_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(hanbali_theater_contemporary, hanbali_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(hanbali_extraction_founding, hanbali_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hanbali_extraction_consolidation, hanbali_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(hanbali_extraction_contemporary, hanbali_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_suppression_founding, hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hanbali_suppression_consolidation, hanbali_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(hanbali_suppression_contemporary, hanbali_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one member of a four-constraint family (Hanbali, Hanafi, Maliki, Shafi'i readings of the usul al-fiqh kernel). Each reading has its own extractiveness value reflecting its specific methodological restrictions and beneficiary/victim structure. The readings are linked via network.affects_constraints because they compete for institutional legitimacy and scholarly authority within the same domain (Sunni Islamic jurisprudence). A shift in one reading's dominance affects the others' resource availability and legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
