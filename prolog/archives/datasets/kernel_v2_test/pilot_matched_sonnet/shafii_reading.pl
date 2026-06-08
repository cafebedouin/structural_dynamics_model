% ============================================================================
% CONSTRAINT STORY: shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shafii_reading, []).

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
 *   constraint_id: shafii_reading
 *   human_readable: Shafi'i Jurisprudential Method: Hadith Hierarchy and Source Restriction
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Shafi'i jurisprudential method, systematized by Imam al-Shafi'i (d.
 *   820 CE) in his Risala, establishes an explicit hierarchy of legal
 *   sources: Qur'an, Sunna (authenticated hadith), ijma' (consensus), and
 *   qiyas (analogical reasoning grounded in transmitted sources). The
 *   method's defining feature is its rejection of juristic preference
 *   mechanisms not grounded in transmitted sources — specifically istihsan
 *   (juristic preference) and maslaha mursala (unrestricted public interest
 *   reasoning) — which were central to Hanafi and Maliki methodologies. This
 *   constraint operates as one reading of the contested kernel 'how to derive
 *   Islamic law from revelation.' The Shafi'i reading elevates hadith
 *   authentication and transmission to the center of legal authority, making
 *   hadith scholars the gatekeepers of legal reasoning. The method solves a
 *   genuine coordination problem (conflicting legal opinions in early Islamic
 *   jurisprudence) but embeds asymmetric extraction: customary practitioners
 *   and rationalist jurists lose epistemic authority, while hadith scholars
 *   gain institutional power. The constraint's extractiveness has increased
 *   over time (0.35 → 0.48) as the method's institutional dominance has
 *   concentrated authority, and its suppression has increased (0.50 → 0.62)
 *   as alternative reasoning mechanisms have been progressively
 *   delegitimized. Theater ratio remains relatively low (0.35) because the
 *   method's source hierarchy is functionally operative, not merely
 *   performative — legal rulings genuinely trace through hadith
 *   authentication, even when the outcome is predetermined by institutional
 *   interests.
 *
 * KEY AGENTS:
 *   - Hadith Scholars: Primary beneficiaries (institutional/arbitrage) — gain gatekeeping authority over legal reasoning through hadith authentication apparatus
 *   - Customary Practitioners: Primary victims (powerless/trapped) — local custom and community practice delegitimized; no alternative institutional pathway
 *   - Istihsan Jurists: Secondary victims (moderate/constrained) — rationalist reasoning methods rejected; can migrate to other madhahib at career cost
 *   - Shafi'i Institutional Hierarchy: Mixed position (institutional/constrained) — benefits from doctrinal coherence but constrained by methodological rigidity
 *   - Comparative Usul Scholars: Organized agents (organized/mobile) — see the method as transitional framework being renegotiated in contemporary ijtihad
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies genuine coordination function alongside asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shafii_reading, 0.48).
domain_priors:suppression_score(shafii_reading, 0.62).
domain_priors:theater_ratio(shafii_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shafii_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(shafii_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shafii_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shafii_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shafii_reading, tangled_rope).
narrative_ontology:human_readable(shafii_reading, "Shafi'i Jurisprudential Method: Hadith Hierarchy and Source Restriction").
narrative_ontology:topic_domain(shafii_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shafii_reading, '27760deb-5aff-400d-b26e-515ac9606b95').
narrative_ontology:cs_kernel_codification('27760deb-5aff-400d-b26e-515ac9606b95', formalized).
narrative_ontology:cs_authority_grounding('27760deb-5aff-400d-b26e-515ac9606b95', lineage).
narrative_ontology:cs_interpretation_layer_present('27760deb-5aff-400d-b26e-515ac9606b95').
narrative_ontology:cs_reading_relation('27760deb-5aff-400d-b26e-515ac9606b95', shafii_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('27760deb-5aff-400d-b26e-515ac9606b95', shafii_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('27760deb-5aff-400d-b26e-515ac9606b95', shafii_reading__hanbali_reading, influences).
narrative_ontology:cs_axiom('27760deb-5aff-400d-b26e-515ac9606b95', foundational, transmitted_source_exclusivity).
narrative_ontology:cs_axiom_status(transmitted_source_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('27760deb-5aff-400d-b26e-515ac9606b95', transmitted_source_exclusivity, deontological).
narrative_ontology:cs_axiom('27760deb-5aff-400d-b26e-515ac9606b95', foundational, hadith_authentication_primacy).
narrative_ontology:cs_axiom_status(hadith_authentication_primacy, holdable).
narrative_ontology:cs_axiom_grounding('27760deb-5aff-400d-b26e-515ac9606b95', hadith_authentication_primacy, conventional).
narrative_ontology:cs_axiom('27760deb-5aff-400d-b26e-515ac9606b95', secondary, istihsan_inadmissibility).
narrative_ontology:cs_axiom_status(istihsan_inadmissibility, holdable).
narrative_ontology:cs_axiom_grounding('27760deb-5aff-400d-b26e-515ac9606b95', istihsan_inadmissibility, deontological).
narrative_ontology:cs_reference_frame('27760deb-5aff-400d-b26e-515ac9606b95', prophetic_sunna_transmitted_completeness).
narrative_ontology:cs_drift_state('27760deb-5aff-400d-b26e-515ac9606b95', contemporary_neo_ijtihad_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27760deb-5aff-400d-b26e-515ac9606b95', '').
narrative_ontology:cs_kernel_id(shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(shafii_reading, textualist_jurists).
narrative_ontology:constraint_beneficiary(shafii_reading, shafii_institutional_hierarchy).
narrative_ontology:constraint_victim(shafii_reading, customary_practitioners).
narrative_ontology:constraint_victim(shafii_reading, local_legal_communities).
narrative_ontology:constraint_victim(shafii_reading, istihsan_jurists).
narrative_ontology:constraint_vindicates(shafii_reading, transmitted_source_supremacy).
narrative_ontology:constraint_vindicates(shafii_reading, hadith_corpus_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hadith scholars authenticate and transmit the prophetic traditions that ground all legal reasoning in the Shafi'i method. They set the agenda by determining which hadiths are sound (sahih) and which are weak (da'if), effectively gatekeeping what counts as valid legal source material. They benefit from the method's elevation of hadith to the center of jurisprudence, gaining institutional authority and scholarly prestige. They can operate across madhahib that respect hadith authority, giving them arbitrage exit.
narrative_ontology:constraint_stakeholder(shafii_reading, hadith_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(shafii_reading, hadith_scholars, beneficiary).

% Jurists who specialize in textual analysis of Qur'an and hadith benefit from the Shafi'i method's rejection of rationalist reasoning mechanisms. Their scholarly function (deriving law from transmitted texts) is elevated, while jurists who rely on istihsan or custom are marginalized. They can migrate to other textualist frameworks (Hanbali, Zahiri) with relatively low cost.
narrative_ontology:constraint_stakeholder(shafii_reading, textualist_jurists, beneficiary,
    institutional, generational, mobile, continental).

% The madhhab's institutional structure (law schools, judicial appointments, fatwa councils) sets the agenda by determining which legal questions are addressed and how. The hierarchy benefits from the method's doctrinal coherence and clear adjudication rules, which preserve institutional identity. However, the strict source hierarchy limits adaptive capacity for novel questions, creating institutional rigidity. Exit is constrained: abandoning the founding methodology would dissolve institutional identity.
narrative_ontology:constraint_stakeholder(shafii_reading, shafii_institutional_hierarchy, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(shafii_reading, shafii_institutional_hierarchy, beneficiary).

% Local legal practitioners who ground their authority in community custom ('urf) and regional practice bear the cost of the Shafi'i method's requirement that all legal reasoning trace to transmitted sources. Their epistemic authority is delegitimized: custom is inadmissible unless validated through hadith. They are trapped where Shafi'i methodology dominates regional legal institutions (Egypt, Southeast Asia, East Africa), with no alternative institutional pathway.
narrative_ontology:constraint_stakeholder(shafii_reading, customary_practitioners, payer,
    powerless, biographical, trapped, regional).

% Communities whose legal norms developed through local practice and custom bear the cost of having their norms invalidated unless they can be traced to transmitted sources. Legal questions that were previously resolved through community consensus now require hadith authentication, concentrating authority in scholars external to the community. They are trapped by the institutional dominance of Shafi'i methodology in their region.
narrative_ontology:constraint_stakeholder(shafii_reading, local_legal_communities, payer,
    powerless, biographical, trapped, local).

% Jurists who use istihsan (juristic preference) or maslaha mursala (unrestricted public interest reasoning) bear the cost of having their reasoning methods formally rejected by the Shafi'i method. They can migrate to Hanafi or Maliki frameworks that accept these mechanisms, but at career cost (retraining, loss of institutional position, geographic relocation). They experience both coordination (the explicit hierarchy does resolve some disputes) and extraction (their methods are delegitimized).
narrative_ontology:constraint_stakeholder(shafii_reading, istihsan_jurists, payer,
    moderate, biographical, constrained, national).

% Scholars working across madhahib to compare and synthesize legal methodologies observe the Shafi'i method as one reading among several. They see the method as a transitional framework whose strict source hierarchy is being renegotiated in contemporary ijtihad. They are organized (academic networks, cross-madhhab conferences) and mobile (can operate in multiple institutional contexts). They neither collect from nor pay into the Shafi'i method directly, but analyze its structure.
narrative_ontology:constraint_stakeholder(shafii_reading, comparative_usul_scholars, observer,
    organized, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Shafi'i method coordinates legal reasoning by establishing an explicit hierarchy of sources (Qur'an, Sunna, ijma', qiyas) that resolves conflicting legal opinions. In early Islamic jurisprudence (8th-9th centuries), different jurists derived contradictory rulings from the same revelatory sources. The Shafi'i systematization provided a framework for adjudicating these conflicts: a ruling grounded in authenticated hadith overrides one grounded in qiyas; consensus overrides individual reasoning. This is a genuine coordination problem: without source hierarchy, legal pluralism becomes legal chaos.
% TRANSFER_FUNCTION: The method transfers epistemic authority from customary practitioners and rationalist jurists to hadith scholars. Customary practitioners lose the ability to ground legal rulings in local practice; rationalist jurists lose the ability to use istihsan or maslaha. Hadith scholars gain gatekeeping power: legal rulings must pass through their authentication apparatus. The transfer is institutional authority and scholarly prestige, from those who reason about law to those who transmit texts about law.
% ABSENT_VOICES: Customary practitioners and local legal communities whose norms were delegitimized by the transmitted-source requirement. These voices were not absent from the founding moment (al-Shafi'i explicitly polemicized against Hanafi istihsan and Maliki custom), but they were structurally disadvantaged: hadith scholars had institutional backing (Abbasid court patronage) while customary practitioners were geographically dispersed. The unanimity of the Shafi'i method within its own institutional tradition arose partly because dissenting seats (customary and rationalist jurists) migrated to other madhahib rather than remaining to contest the Shafi'i reading internally.
% DISAPPEARANCE_RATIONALE: If the Shafi'i method disappeared overnight, legal reasoning in Shafi'i-dominant regions would rearrange itself. Hadith scholars would lose gatekeeping authority; customary practitioners and rationalist jurists would regain epistemic standing; legal rulings would be grounded in custom, public interest, and juristic preference rather than exclusively in transmitted sources. The institutional hierarchy (law schools, judicial appointments, fatwa councils) is organized around the Shafi'i source hierarchy; removing the constraint would force reorganization. This is not a natural fact (legal reasoning does not inherently require transmitted-source exclusivity) but an institutional arrangement.
% FOUNDING_PROBLEM: Conflicting legal opinions in early Islamic jurisprudence (8th-9th centuries CE). Different jurists derived contradictory rulings from the same revelatory sources (Qur'an and hadith), and there was no agreed framework for adjudicating these conflicts. Regional legal traditions (Iraqi rationalism, Hijazi traditionalism) used different reasoning mechanisms, producing legal pluralism that threatened the unity of Islamic law. Al-Shafi'i's Risala systematized legal theory to resolve this: an explicit source hierarchy that prioritized transmitted sources (Qur'an, authenticated hadith) over rationalist mechanisms (istihsan, custom).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (conflicting legal opinions requiring systematization) is corroborated by historical sources outside the Shafi'i tradition: Hanafi and Maliki jurists also recognized the need for usul al-fiqh systematization, though they proposed different solutions. However, whether the problem is still live is contested. Shafi'i institutional authorities claim the problem persists (legal reasoning still requires source hierarchy to prevent chaos). Comparative usul scholars and neo-ijtihad movements argue the problem has shifted: contemporary legal questions (bioethics, finance, technology) require adaptive reasoning mechanisms that the strict Shafi'i hierarchy cannot accommodate, suggesting the founding problem (conflicting opinions) has been replaced by a new problem (methodological rigidity).
narrative_ontology:disappearance_verdict(shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(shafii_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL CUSTOMARY PRACTITIONERS (SNARE) — Trapped by the requirement that all legal reasoning trace to transmitted sources. Local custom ('urf) and community practice, which previously grounded legal authority, are now inadmissible unless validated through hadith. Cannot exit the Shafi'i framework where it dominates regional legal institutions. Maximum extraction: their epistemic authority is delegitimized without alternative institutional pathway.
constraint_indexing:constraint_classification(shafii_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ISTIHSAN JURISTS (TANGLED ROPE) — Constrained by the rejection of juristic preference (istihsan) and public interest reasoning (maslaha mursala) not grounded in transmitted sources. Experience both coordination (the explicit hierarchy does resolve some interpretive disputes) and extraction (their reasoning methods are delegitimized). Can migrate to Hanafi or Maliki frameworks but at career cost. Mixed experience: the system coordinates legal reasoning while extracting from non-textualist methodologies.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HADITH SCHOLARS (ROPE) — Primary beneficiaries. The Shafi'i method elevates hadith transmission and authentication to the center of legal authority. Hadith scholars become gatekeepers: legal rulings must pass through their authentication apparatus. Experience the constraint as coordination: the explicit hierarchy solves the genuine problem of conflicting legal opinions by grounding all reasoning in transmitted sources. Net beneficiary with arbitrage exit: can operate across madhahib that respect hadith authority.
constraint_indexing:constraint_classification(shafii_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPARATIVE USUL SCHOLARS (SCAFFOLD) — Organized scholars working across madhahib see the Shafi'i method as a transitional framework that systematized legal theory in the 9th century but whose strict source hierarchy is being renegotiated in contemporary ijtihad. The sunset is implicit: as legal pluralism and cross-madhhab synthesis mature, the strict rejection of istihsan and maslaha becomes untenable for addressing novel questions (bioethics, finance, technology). Estimated sunset: ongoing over 50-100 years as neo-ijtihad movements mature.
constraint_indexing:constraint_classification(shafii_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SHAFI'I INSTITUTIONAL HIERARCHY (TANGLED ROPE) — The madhhab's own institutions experience both coordination (the method provides clear adjudication rules and preserves doctrinal coherence) and extraction (the strict source hierarchy limits adaptive capacity for novel questions, creating institutional rigidity). Constrained exit: cannot abandon the founding methodology without dissolving institutional identity, but the methodology's limitations create pressure for informal workarounds.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The Shafi'i method solves a genuine coordination problem (conflicting legal opinions in early Islamic jurisprudence) through explicit source hierarchy, but embeds asymmetric extraction: hadith scholars gain institutional authority while customary and rationalist jurists lose it. The method is neither pure coordination (rope) nor pure extraction (snare) but a hybrid that coordinates legal reasoning while concentrating epistemic authority in the hadith transmission apparatus. The claimed type matches the computed type from this perspective.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shafii_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shafii_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shafii_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The Shafi'i method concentrates epistemic authority in hadith scholars while delegitimizing customary and rationalist reasoning. The extraction is real but not maximal — the method does solve a coordination problem (conflicting legal opinions), and hadith authentication is a genuine scholarly function, not pure rent-seeking. The value reflects that the career and institutional asymmetry is substantial but partly justified by the coordination function. Suppression (0.62): Moderate-high. Significant barriers to alternative reasoning mechanisms: istihsan and maslaha are formally rejected, customary practice requires hadith validation, and the institutional dominance of Shafi'i methodology in certain regions (Egypt, Southeast Asia, East Africa) makes exit costly. But suppression is not total — other madhahib coexist, and informal workarounds exist within Shafi'i practice. Theater ratio (0.35): Low-moderate. The method's source hierarchy is functionally operative: legal rulings genuinely trace through hadith authentication, and the explicit hierarchy does resolve interpretive disputes. Theater exists (some hadith authentication is post-hoc legitimation of predetermined rulings) but is not dominant. The ratio has increased modestly over time as institutional interests have layered onto the methodology.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across five types. Hadith scholars see coordination (Rope) — the explicit hierarchy solves the problem of conflicting opinions and elevates their scholarly function. Customary practitioners see pure extraction (Snare) — their epistemic authority is delegitimized with no exit. Istihsan jurists and the Shafi'i institutional hierarchy see mixed coordination and extraction (Tangled Rope) — the system both coordinates and constrains. Comparative usul scholars see a transitional framework (Scaffold) — the strict source hierarchy is being renegotiated as legal pluralism matures. The analytical observer sees Tangled Rope: genuine coordination (the method does resolve interpretive disputes) alongside asymmetric extraction (hadith scholars gain institutional power). The perspectival gap is structural: the same source hierarchy appears as coordination, extraction, or both, depending on the agent's position in the epistemic authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Hadith scholars are beneficiaries with arbitrage exit (can operate across madhahib) → low d → low/negative chi (experience as coordination). Customary practitioners are victims with trapped exit → high d → high chi (experience as extraction). Istihsan jurists are victims with constrained exit → moderate-high d → moderate chi (mixed experience). The Shafi'i institutional hierarchy is a beneficiary with constrained exit (cannot abandon founding methodology without dissolving identity) → moderate d → moderate chi (mixed experience). The analytical observer has analytical exit and sees the full structure → moderate d reflecting the genuine coordination function alongside extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the structurally accurate classification from the analytical perspective: the Shafi'i method solves a genuine coordination problem (conflicting legal opinions in early Islamic jurisprudence required systematization) while embedding asymmetric extraction (hadith scholars gain institutional authority at the expense of customary and rationalist jurists). The method is neither pure coordination (it delegitimizes alternative reasoning mechanisms) nor pure extraction (it does provide a functional source hierarchy that resolves interpretive disputes). The mandate (systematizing legal theory) has not outlived its function — legal reasoning still requires source hierarchy — but the specific Shafi'i hierarchy concentrates authority in ways that benefit hadith scholars. The perspectival gap (Rope from beneficiaries, Snare from victims, Tangled Rope from analytical observer) is the diagnostic signature of a hybrid constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_corpus_completeness,
    'Does the hadith corpus contain sufficient transmitted material to address all legal questions, or does the rejection of istihsan/maslaha create irresolvable gaps?',
    'Historical analysis of novel legal questions (post-9th century) and whether Shafi''i jurists could address them without informal recourse to reasoning mechanisms formally rejected by the method. Comparison with Hanafi/Maliki adaptive capacity.',
    'If corpus is complete: the method is genuine coordination (rope from more perspectives). If gaps exist: the method is extraction that forces legal questions into a Procrustean bed of transmitted sources, with hadith scholars as gatekeepers to workarounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_corpus_completeness, empirical, 'Whether hadith corpus suffices for all legal questions').

omega_variable(
    committer_frame_kernel,
    'Is the Shafi''i reading one interpretation of a contested kernel (usul al-fiqh method), or is it the uniquely correct systematization of Islamic legal theory?',
    'Recognition that Hanafi, Maliki, and Hanbali readings coexist as live jurisprudential frameworks with different source hierarchies and reasoning mechanisms. The kernel (how to derive law from revelation) admits multiple readings; the Shafi''i reading is one structural instantiation.',
    'If one reading: the constraint is a mountain (natural law of Islamic jurisprudence). If multiple readings: the constraint is one reading of a contested kernel, and the extraction/coordination balance is reading-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel, conceptual, 'Whether Shafi''i method is one reading of contested kernel').

omega_variable(
    istihsan_suppression_mechanism,
    'Is the rejection of istihsan a principled methodological commitment or a strategic move to concentrate authority in hadith scholars?',
    'Historical analysis of al-Shafi''i''s polemics against Hanafi istihsan: were the arguments epistemological (istihsan lacks transmitted grounding) or institutional (istihsan empowers jurists at the expense of hadith scholars)? Examination of whether Shafi''i jurists informally use istihsan-like reasoning under different labels.',
    'If principled: lower extraction (the method is coordination with side effects). If strategic: higher extraction (the method is institutional capture disguised as epistemology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_suppression_mechanism, empirical, 'Whether istihsan rejection is principled or strategic').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the explicit usul al-fiqh methodology (the formal rules), or the implicit legitimacy claim (that law must trace to revelation)?',
    'Two coherent framings: (1) kernel = the formal source hierarchy and reasoning rules (explicit methodology); (2) kernel = the deeper claim that Islamic law is discovered from revelation, not constructed by jurists (implicit legitimacy). The first framing makes the readings methodological variants; the second makes them competing claims about the nature of legal authority.',
    'Framing (1): readings coexist as technical alternatives. Framing (2): readings foreclose each other because they make incompatible claims about whether law is transmitted or constructed. The Shafi''i reading under framing (2) forecloses rationalist readings more strongly than under framing (1).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether kernel is explicit methodology or implicit legitimacy claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shafii_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_theater_founding, shafii_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(shafii_theater_consolidation, shafii_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(shafii_theater_contemporary, shafii_reading, theater_ratio, 600, 0.35).

% Extraction over time
narrative_ontology:measurement(shafii_extract_founding, shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shafii_extract_consolidation, shafii_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(shafii_extract_contemporary, shafii_reading, base_extractiveness, 600, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(shafii_suppress_founding, shafii_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(shafii_suppress_consolidation, shafii_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(shafii_suppress_contemporary, shafii_reading, suppression_requirement, 600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Shafi'i reading is one of four major readings of the usul al-fiqh kernel. Each reading has its own extractiveness value reflecting its specific source hierarchy and beneficiary structure. The Shafi'i reading's extractiveness (0.48) reflects the concentration of authority in hadith scholars; the Hanafi reading's extractiveness would reflect different beneficiaries (rationalist jurists, customary practitioners). The readings are linked via network.affects_constraints because the Shafi'i systematization of usul al-fiqh as a discipline influenced how other madhahib articulated their methodologies, even when they rejected specific Shafi'i positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
