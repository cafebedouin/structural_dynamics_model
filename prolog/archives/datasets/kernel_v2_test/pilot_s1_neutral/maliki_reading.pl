% ============================================================================
% CONSTRAINT STORY: maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maliki_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Practice and Public Interest Authority
 *   domain: islamic_jurisprudence/legal_theory/usul_al_fiqh
 *
 * SUMMARY:
 *   The Maliki jurisprudential school's methodological privilege of Medina
 *   customary practice ('amal) and contextual public interest (maslaha
 *   mursala) as authoritative sources alongside Quran and Hadith constitutes
 *   one reading of the broader Islamic jurisprudence kernel (usul al-fiqh).
 *   This reading is structurally distinct from its sibling schools (Hanafi
 *   literalism, Shafi'i principled synthesis, Hanbali hadith-primacy) in the
 *   sources it elevates and the beneficiaries it creates. The constraint
 *   exhibits a tangled coordination-extraction hybrid: the Maliki framework
 *   solves a genuine jurisprudential coordination problem (how to make law
 *   responsive to actual community conditions while maintaining Islamic
 *   grounding), yet simultaneously creates institutional advantages for
 *   practitioners embedded in the Maliki apparatus and disadvantages for
 *   hadith-literalists committed to text-centered methodology. Extractiveness
 *   (0.35) reflects moderate asymmetry: the benefit flows primarily to Maliki
 *   institutional practitioners and regional customary authorities, while the
 *   cost falls on those committed to alternative methodologies who find
 *   themselves working within an institutional structure that privileges
 *   Maliki sources. Theater ratio (0.42) is moderate, indicating that while
 *   the Maliki method has active jurisprudential deployment in some contexts,
 *   much contemporary teaching and transmission is ceremonial rather than
 *   functionally determinative of actual legal outcomes in modern
 *   nation-states.
 *
 * KEY AGENTS:
 *   - Maliki Legal Institution: Institutional beneficiary (institutional/arbitrage) — gains authority and institutional survival through practice-based flexibility
 *   - Regional Customary Authorities: Secondary beneficiary (organized/constrained) — legitimacy for local practice is jurisprudentially grounded through 'amal authority
 *   - Hadith Literalist Jurists: Primary victim (powerless/trapped) — methodologically committed to hadith-primacy but institutionally pressured within Maliki schools to deploy maslaha and 'amal
 *   - Practicing Qadi / Regional Judge: Moderate agent (moderate/constrained) — genuine benefit from coordination function enabling locally-responsive judgment, but also bears extraction through discretionary authority asymmetries
 *   - Hanafi, Shafi'i, Hanbali Schools: Competing institutional actors (institutional/mobile) — constrained by Maliki regional dominance in some territories, but retain methodological autonomy in others
 *   - Comparative Legal Theory Movement: Organized observers (organized/mobile) — view the constraint as historically productive but temporally bounded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent methodological choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maliki_reading, 0.35).
domain_priors:suppression_score(maliki_reading, 0.28).
domain_priors:theater_ratio(maliki_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maliki_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(maliki_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(maliki_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maliki_reading, tangled_rope).
narrative_ontology:human_readable(maliki_reading, "Maliki Jurisprudential Method: Practice and Public Interest Authority").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_theory/usul_al_fiqh").

domain_priors:requires_active_enforcement(maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, 'aeaa2057-adb3-4bc6-9b8d-4859b242b15f').
narrative_ontology:cs_kernel_codification('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', fixed_text).
narrative_ontology:cs_authority_grounding('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', lineage).
narrative_ontology:cs_interpretation_layer_present('aeaa2057-adb3-4bc6-9b8d-4859b242b15f').
narrative_ontology:cs_reading_relation('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', maliki_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', maliki_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', maliki_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', foundational, medina_practice_as_authoritative_source).
narrative_ontology:cs_axiom_status(medina_practice_as_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', medina_practice_as_authoritative_source, conventional).
narrative_ontology:cs_axiom('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', foundational, contextual_public_interest_reasoning_legitimate).
narrative_ontology:cs_axiom_status(contextual_public_interest_reasoning_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', contextual_public_interest_reasoning_legitimate, instrumental).
narrative_ontology:cs_reference_frame('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', maliki_medina_precedent_authority).
narrative_ontology:cs_drift_state('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aeaa2057-adb3-4bc6-9b8d-4859b242b15f', '2026-02-26T14:30:00Z').
narrative_ontology:cs_kernel_id(maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, maliki_legal_practitioners).
narrative_ontology:constraint_beneficiary(maliki_reading, regional_customary_authorities).
narrative_ontology:constraint_beneficiary(maliki_reading, maslaha_invoking_jurists).
narrative_ontology:constraint_victim(maliki_reading, hadith_literalist_tradition).
narrative_ontology:constraint_victim(maliki_reading, text_centered_methodologies).
narrative_ontology:constraint_victim(maliki_reading, non_medina_regional_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maliki_reading, regional_customary_keepers).
narrative_ontology:constraint_beneficiary(maliki_reading, practicing_qadis).
narrative_ontology:constraint_victim(maliki_reading, hadith_literalist_scholars).
narrative_ontology:constraint_victim(maliki_reading, practicing_qadis).
narrative_ontology:constraint_victim(maliki_reading, rival_school_practitioners).
narrative_ontology:constraint_vindicates(maliki_reading, practice_based_authority).
narrative_ontology:constraint_vindicates(maliki_reading, contextual_public_interest).
narrative_ontology:constraint_vindicates(maliki_reading, regional_jurisprudential_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The formal Maliki legal apparatus (madrasas, juridical colleges, fatwa-issuing institutions) sets and enforces the methodological standards that define legitimate Maliki jurisprudence. They codify which precedents count as 'amal, which contexts justify maslaha invocation, and which scholarly syntheses become authoritative. They benefit from institutional authority and relevance that the practice-based and maslaha framework provides. They can and do migrate these frameworks to different regions and contexts, maintaining methodological flexibility.
narrative_ontology:constraint_stakeholder(maliki_reading, maliki_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Local authorities, community elders, and practice-keepers whose actual customs and decisions are elevated to the status of 'amal (jurisprudentially authoritative regional practice) within the Maliki framework. They gain legitimacy and grounding that positions their authority as Islamic and methodologically sound. They cannot freely exit this legitimation — once their practice is recognized as 'amal, they face expectation that it will be consistent and judicially reviewable.
narrative_ontology:constraint_stakeholder(maliki_reading, regional_customary_keepers, beneficiary,
    organized, generational, constrained, regional).

% Scholars committed to hadith-centered jurisprudence who find themselves within Maliki institutional structures (law schools, qadi training) face pressure to learn and deploy maslaha reasoning and 'amal citation even when their methodological commitment is to evaluate claims against explicit Quranic and hadith sources. Their scholarly integrity requires principled objection, but institutional advancement requires appearing competent in Maliki methods. Regional dominance of Maliki institutions in their areas makes exit costly (leaving the school means leaving the jurisdiction's jurisprudential community).
narrative_ontology:constraint_stakeholder(maliki_reading, hadith_literalist_scholars, payer,
    powerless, biographical, trapped, regional).

% Regional judges who must resolve actual disputes face genuine coordination problem: novel situations, local context, and practical outcomes require flexibility that text alone cannot provide. The Maliki framework enables this flexibility through maslaha and 'amal, allowing judgment responsive to community needs while maintaining Islamic jurisprudential grounding. They benefit from the methodological tools provided. But they also face extraction: the discretionary authority embedded in maslaha reasoning creates opportunities for politically-connected litigants to secure favorable rulings, and judges bear accountability for their discretionary choices in ways they might not if they were purely applying explicit rules.
narrative_ontology:constraint_stakeholder(maliki_reading, practicing_qadis, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maliki_reading, practicing_qadis, payer).

% Scholars and institutions committed to Hanafi, Shafi'i, or Hanbali methodologies operate in regions where Maliki jurisprudence has institutional dominance. They face structural disadvantage: their methodological commitments are not recognized as authoritative by regional legal institutions, their students face pressure to learn Maliki methods, and their legal opinions carry less weight in qadi courts. They can exit through migration or maintaining separate institutional structures, but at cost to regional influence.
narrative_ontology:constraint_stakeholder(maliki_reading, rival_school_practitioners, payer,
    institutional, generational, mobile, regional).

% Nation-states implementing secular legal codes and constitutional governance have largely displaced traditional Islamic jurisprudential authority as the basis for law. Some states recognize Islamic jurisprudence in family law or minority-religion contexts, but the Maliki method is invoked more as cultural-historical reference than as operative legal methodology. Modern states have the power and mobility to establish legal systems independent of Islamic jurisprudential frameworks.
narrative_ontology:constraint_stakeholder(maliki_reading, modern_secular_states, observer,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_non_agent(maliki_reading, modern_secular_states).

% The underlying methodological commitment of Islamic jurisprudence to ground legal reasoning in authoritative sources (Quran, Hadith, Consensus, Analogy) is itself an abstract commitment that none of the schools can exit from without ceasing to be Islamic jurisprudential schools. The kernel persists across all readings, and the constraint operates within this persistent frame.
narrative_ontology:constraint_stakeholder(maliki_reading, islamic_jurisprudential_kernel, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(maliki_reading, islamic_jurisprudential_kernel).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Medina customary practice and contextual public interest reasoning enable qadis to address novel disputes and local conditions while maintaining Islamic jurisprudential legitimacy grounding. The genuine coordination problem: how to apply seventh-century textual sources (Quran, Hadith) to eighth-century and later contexts without either abandoning legal authority or freezing law in time. The Maliki method solves this through 'amal (established precedent of Medina as model community) and maslaha (contextual reasoning about outcomes that serve public welfare). This allows legal systems to adapt while claiming fidelity to Islamic sources.
% TRANSFER_FUNCTION: The constraint transfers authority from text-literalists to practice-based jurists. It moves jurisprudential legitimacy from those who ground reasoning in explicit Quranic verses and authenticated hadith reports to those who can invoke regional practice and public-interest considerations. It transfers prestige and institutional position toward Maliki-trained scholars and away from hadith-purists. In material terms, it transfers advantage in qadi appointment and fatwa-issuing authority toward practitioners embedded in Maliki institutional structures.
% ABSENT_VOICES: Text-literalist voices within Islamic jurisprudence are not eliminated but are systematically marginalized in Maliki-dominant regions. Post-colonial legal scholars questioning whether Islamic jurisprudence should have modern legal authority at all are excluded from the conversation (their framing assumes Islamic jurisprudence is displaced). Secular constitutional frameworks that have replaced Islamic jurisprudential authority are absent from the constraint's internal discourse — the constraint operates as if Islamic jurisprudential methods remain operative authorities, even in contexts where they have been displaced.
% DISAPPEARANCE_RATIONALE: In regions where Maliki jurisprudence remains institutionally embedded (some Islamic courts, fatwa-issuing bodies, family law systems), if the constraint disappeared (if maslaha and 'amal were suddenly no longer recognized as authoritative sources), the world would rearrange: qadi courts would face unprecedented rigidity in applying law to novel situations, legal legitimacy would suffer through inability to address community context, institutional authority would collapse. In regions where secular nation-states have displaced Islamic jurisprudential authority, if the constraint disappeared, no rearrangement would occur — the legal system would continue unchanged. The contested nature reflects that the constraint's operative status varies by jurisdiction.
% FOUNDING_PROBLEM: The founding problem was genuine: in the 8th-9th centuries CE, Islamic jurisprudence faced the problem of extending seventh-century prophetic and Quranic guidance to novel situations, unfamiliar legal questions, and culturally diverse communities. The Quran and authenticated hadith collections did not contain explicit guidance for many disputes. Strict literalism would mean either making law up (contradiction) or declaring problems unsolvable (legal paralysis). The Maliki school's solution: recognize 'amal (established practice of Medina and its authorities as a form of living precedent) and maslaha (public-interest reasoning as legitimate jurisprudential method) as supplementary authoritative sources. This enabled law to remain Islamic while being responsive to actual conditions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: comparative legal history (other legal systems solving the novel-situation problem through different mechanisms), post-colonial legal theory (explicit documentation of displacement of Islamic jurisprudence by modern legal systems), institutional observation (modern Islamic-law practicing courts operating under constitutional authority rather than through independent jurisprudential methodology). Not corroborated by any source within the Maliki apparatus itself — beneficiaries will naturally resist the claim that the founding problem is solved.
narrative_ontology:disappearance_verdict(maliki_reading, contested).
narrative_ontology:founding_problem_status(maliki_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HADITH LITERALIST JURIST (SNARE) — Trapped within a regional jurisprudential framework that has institutionalized maslaha invocation and customary practice as co-equal authorities. A scholar committed to hadith-based reasoning without explicit public-interest recourse faces delegitimization within Maliki institutional structures. No exit option: rejecting the framework means leaving the school entirely. Maximum extraction — the constraint forces adoption of methods the agent rejects on principle.
constraint_indexing:constraint_classification(maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRACTICING QADI / REGIONAL JUDGE (TANGLED ROPE) — Faces genuine coordination problem: settlements and disputes require authority figures to weigh community practice and public welfare alongside textual sources. The Maliki framework enables this coordination function. But also experiences extraction: flexibility in maslaha invocation creates discretionary authority that can disadvantage those without political patronage. Mixed experience: real coordination benefit + asymmetric enforcement power.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALIKI LEGAL INSTITUTION (ROPE) — Benefits from methodological flexibility: the practice-based and maslaha framework allows adaptation to local conditions without formal revision of fiqh foundations. Institutional survival depends on perceived relevance to actual disputes. Experiences the constraint as enabling coordination: can address novel situations by invoking public interest without doctrinal upheaval. Net beneficiary — coordination and institutional authority flow together.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL CUSTOMARY AUTHORITY COLLECTIVE (TANGLED ROPE) — Organized groups of local practice-keepers and tradition-maintainers benefit from Maliki method's legitimacy given to 'amal (regional practice). Their authority becomes jurisprudentially grounded rather than merely customary. But also face constraint: once 'amal is institutionalized as a source, regional authorities are bound by judicial review of their practice claims — they cannot operate purely outside the framework. Coordination benefit (legitimacy) + extraction (accountability).
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COMPARATIVE LEGAL THEORY MOVEMENT (SCAFFOLD) — Post-colonial legal scholarship sees the Maliki reading as a historical solution to a transitional problem: how to ground regional legal authority in Islamic tradition while respecting local practice. The framework's sunset: as modern nation-state legal systems emerge and global legal harmonization pressures intensify, the practice-based Maliki method becomes less relevant as a basis for state law. Viewed as productive but temporary — the constraint served its coordination function but is being superseded by secular constitutionalism and codified law. Mobile exit available: scholars move to comparative constitutional law.
constraint_indexing:constraint_classification(maliki_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL MALIKI JURISPRUDENTIAL APPARATUS (PITON) — The formal teaching and transmission structures of Maliki fiqh in madrasas and universities largely maintain the method through theatrical re-enactment: students learn the canonical texts invoking maslaha and 'amal, yet actual modern application in secular nation-states rarely relies on these sources. The apparatus persists through institutional inertia and scholarly convention rather than functional deployment. Theater ratio high: the method is preserved as an authoritative framework more than as a lived jurisprudential practice in most contemporary legal systems. Theater-gate classification applies.
constraint_indexing:constraint_classification(maliki_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TRANSCENDENTAL VIEW (MOUNTAIN) — From a civilizational perspective, the Maliki privileging of regional practice and contextual public interest reflects an inherent feature of legal authority: all law must balance textual stability with practical applicability to particular communities. The constraint appears as an immutable principle of jurisprudence itself — any legal system must reconcile universal principles with local context. However, this naturalization risks obscuring the contingent choice the Maliki school made to institutionalize both sources with specific authority weights. The engine's false summit detector will identify this as perspectival naturalization of a constructed methodological choice.
constraint_indexing:constraint_classification(maliki_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maliki_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maliki_reading, TR),
    TR >= 0.70.

:- end_tests(maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Maliki privileging of 'amal and maslaha creates genuine coordination benefit for qadis and regional authorities who need to address novel situations. But the extraction component is substantial: those methodologically committed to hadith-literalism face institutional delegitimization, and the discretionary authority embedded in maslaha invocation creates asymmetric advantages for politically well-connected practitioners. The value reflects that this is not pure extraction (there is real coordination benefit) but also not pure coordination (there are identifiable victims and institutional capture). Suppression (0.28): Moderate-low. The constraint operates through methodological authority and institutional credential rather than through coercion. Hadith-literalists can in principle maintain their commitments and leave Maliki institutions, though career and regional-authority costs are real. Exit is possible but carries costs. Theater ratio (0.42): Moderate-low. The Maliki method is actively deployed in fatwa production and qadi reasoning in many contexts, but contemporary deployment of maslaha in modern secular nation-states is rare — the apparatus maintains the framework more through scholarly transmission than through functional application. The rising trajectory reflects that as modern legal systems have displaced traditional jurisprudential authority, the theater component has increased relative to functional deployment.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the institutional Maliki beneficiary (Rope) and the trapped hadith-literalist (Snare) is maximal. The beneficiary experiences the constraint as enabling coordination and institutional relevance; the victim experiences it as forced adoption of methods they reject on principle. The qadi (Tangled Rope) occupies an intermediate position: genuine coordination benefit but also extraction through discretionary authority. The regional customary authority collective (Tangled Rope) benefits from legitimacy but faces institutional accountability constraints they did not previously bear. The analytical observer risks naturalizing the constraint as an immutable principle of law, when it is actually a specific institutional choice the Maliki school made. This perspectival gap is the diagnostic signature: what appears natural from one perspective appears constructed and extractive from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the constraint. Maliki institutional practitioners are beneficiaries with arbitrage-level exit (high mobility across Islamic jurisprudential institutions); they experience low effective extraction (negative χ). Hadith-literalists trapped within Maliki institutions are victims with trapped-level exit; they experience high effective extraction (high χ). Regional customary authorities are beneficiaries with constrained exit (they benefit from legitimacy but face accountability); they experience moderate-to-low effective extraction. The qadi balances beneficiary status (coordination benefit) with victim-like constraints (accountability and discretionary authority burdens); mixed d produces tangled_rope. The competing schools have mobile exit (they can maintain their methodologies elsewhere) and derive no institutional benefit from Maliki primacy; they experience moderate negative extraction (barriers to influence in Maliki-dominated regions). Directionality is NOT overridden in this story; structural derivation from beneficiary/victim + exit options + power atom produces appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY NOT RESOLVED: The constraint's original mandate was to ground legal authority in both textual sources and community practice — to preserve Islamic jurisprudential legitimacy while adapting to local conditions. This mandate remains live in contexts where Maliki jurisprudence is actively deployed (some Islamic courts, fatwa-issuing institutions). However, the constraint exhibits emerging mandatrophy in modern nation-states: secular legal systems have displaced traditional jurisprudential authority, and the practice-based and maslaha frameworks are invoked more as cultural-historical references than as operative legal methodologies. The piton perspective reveals that the apparatus maintains itself through theatrical transmission (madrasa teaching, scholarly publications) rather than through functional application in actual dispute resolution. This suggests a pathway to resolution: either (1) the Maliki framework adapts to modern legal pluralism contexts (constitutional pluralism, comparative law integration), maintaining functional relevance; or (2) the apparatus becomes purely heritage-maintenance (historical study of Islamic jurisprudence) with no pretense to operative authority. The constraint is poised between these trajectories; mandatrophy will be resolved when the trajectory clarifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maslaha_scope_ambiguity,
    'What constitutes legitimate ''public interest'' (maslaha mursala) invocation versus disguised judicial discretion serving particular factions?',
    'Historical analysis of maslaha fatwa patterns: correlation between invoked public interest and actual distributional outcomes; comparison with parallel Hanafi and Shafi''i schools to identify whether maslaha produces systematically different outcomes for similar disputes',
    'If maslaha is coherently principled: Maliki reading maintains rope/tangled_rope classification. If maslaha is functionally discretionary cover: reclassifies to snare (victims are those without patronage to influence discretionary reasoning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_scope_ambiguity, empirical, 'Whether maslaha constitutes coherent methodology or discretionary cover').

omega_variable(
    amal_authentic_precedent_versus_constructed,
    'Is ''amal (Medina customary practice) a genuinely transmitted corpus of actual historical practice, or a reconstructed/idealized body of precedent authored by Maliki jurists seeking methodological legitimacy?',
    'Hadith-critical analysis comparing ''amal citations in Maliki texts to independent historical sources on Medina practice; examination of whether ''amal attributions pre-date Maliki codification or emerge post-hoc in authoritative Maliki texts',
    'If genuinely transmitted: the practice-primacy axiom holds weight as grounding in actual regional authority. If constructed: the beneficiary structure shifts — maslaha and ''amal become tools for Maliki institutional builders rather than expressions of authentic regional custom, reclassifying as institutional self-strengthening (higher extraction from text-literalists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amal_authentic_precedent_versus_constructed, empirical, 'Whether ''amal is authentic historical practice or Maliki jurisprudential construction').

omega_variable(
    regional_versus_universal_authority_boundary,
    'Does the Maliki legitimation of regional practice create a coherent principle of subsidiarity (local authority for local questions), or does it establish a competing universalism where Maliki-approved practice authority can override other schools'' universalist claims?',
    'Comparative examination of how Maliki institutions adjudicate disputes between their own regional practice rulings and Hanafi or Shafi''i universal-principle rulings in multi-school jurisdictions; analysis of whether ''amal-based rulings expand beyond their originating region or remain localized',
    'If genuinely subsidiarity: the constraint is a coordination mechanism (stronger rope classification). If competing universalism: the Maliki reading becomes a competitive claim-staker against other schools (higher extraction through institutional dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_versus_universal_authority_boundary, conceptual, 'Whether practice-primacy establishes subsidiarity or competing universalism').

omega_variable(
    kernel_reading_contingency,
    'Is this Maliki reading (practice + maslaha primacy) one coherent reading of usul al-fiqh method, or does it represent multiple independent methodological choices (practice authority ≠ maslaha authority) that have been bundled by institutional Maliki transmission?',
    'Textual analysis of foundational Maliki texts (al-Shatibi, al-Qarafi, Sahnun) to determine whether practice and maslaha are presented as integrated methodological pair or as separable principles. Comparison to other schools'' treatment of each source independently.',
    'If integrated: this constraint is one stable reading (current modeling). If separable: should decompose into two constraints (one for ''amal authority, one for maslaha authority) with potentially different ε values and beneficiary structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether practice-primacy and maslaha-authority are integrated or separable methodological principles').

omega_variable(
    false_summit_mountain_test,
    'Is the analytical ''transcendental principle'' view (Perspective 7) a genuine natural law of jurisprudence, or does it naturalize a specific institutional choice made by the Maliki school?',
    'Comparative survey of other legal systems (common law, civil law, traditional customary law systems, post-colonial states) to determine whether all legal systems necessarily privilege practice and public interest as co-equal authorities. Identification of counter-examples (purely textual traditions, purely precedent-based systems, purely legislative systems) would suggest the mountain classification is a false summit.',
    'If genuinely universal: mountain classification holds. If contingent to Maliki institutional choice: engine''s false summit detector reclassifies to tangled_rope (naturalization is itself an extraction mechanism — it forecloses recognition that other methodological choices are possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_test, empirical, 'Whether transcendental principle view is genuine natural law or constructed naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(malik_tr_t0, maliki_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(malik_tr_t3, maliki_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(malik_tr_t6, maliki_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(malik_tr_t9, maliki_reading, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(malik_be_t0, maliki_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(malik_be_t3, maliki_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(malik_be_t6, maliki_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(malik_be_t9, maliki_reading, base_extractiveness, 9, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(malik_su_t0, maliki_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(malik_su_t5, maliki_reading, suppression_requirement, 5, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maliki_reading, 0.12).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).
narrative_ontology:affects_constraint(maliki_reading, islamic_jurisprudence_institutional_authority).
narrative_ontology:affects_constraint(maliki_reading, qadi_discretionary_judgment_space).

% DUAL FORMULATION NOTE:
% The Maliki reading is part of a constraint family spanning four school readings of the usul al-fiqh kernel. Each reading has distinct beneficiary/victim structures and extractiveness values. The Maliki reading (0.35 extractiveness) is intermediate: higher extraction than the Shafi'i reading (which attempts to balance sources) and lower than the Hanbali reading (which imposes strict constraints on discretion). Decomposition follows the ε-invariance principle: each school's reading has a distinct methodology that produces different outcomes for actual disputes — these are not the same constraint viewed from four angles, but four structurally distinct constraints linked by common kernel. Each reading also influences practical jurisprudential outcomes in actual qadi courts and fatwa-issuing bodies; these downstream constraints are separately modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
