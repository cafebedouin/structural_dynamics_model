% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Progressive-Synthesis Reading of Verse 9:5 as Superseded Historical Directive
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This story authors the progressive-synthesis reading of verse 9:5 as a
 *   distinct constraint from its sibling readings (abrogating_universal,
 *   contextual_defensive), per the ε-invariance principle: each reading of
 *   the kernel produces a structurally different claim about who is bound by
 *   what, and averaging across them would misrepresent all three. Under
 *   progressive-synthesis, verse 9:5 is understood as a time-bound
 *   7th-century political-military directive addressed to a specific
 *   historical situation (treaty-breaking polytheist tribes in Medina), whose
 *   ethical rationale has been superseded by the Quran's broader trajectory
 *   toward justice and pluralism (Fazlur Rahman's 'double movement' and
 *   cognate maqasid approaches). The reading itself functions institutionally
 *   as a constraint on textualist authority claims: it does not bind
 *   believers to any behavior toward polytheists or treaty-partners, but it
 *   does constrain what textualist institutions can claim scripture requires.
 *   The extraction this story measures is the reading's effect on
 *   institutions whose authority structure depends on the verse retaining
 *   binding force, not extraction from ordinary believers, who under this
 *   reading are simply unconstrained by 9:5's directive.
 *
 * KEY AGENTS:
 *   - textualist_authority_structures: institutional target — loses ground for claims requiring 9:5's ongoing bindingness
 *   - traditional_madrasa_jurists: professional target — abrogation-doctrine training devalued
 *   - reformist_muslim_scholars: agenda-setters — author and circulate the historicist reading
 *   - secular_pluralist_legal_frameworks: institutional beneficiary — gains a hermeneutic ally against literalist coercion claims
 *   - interfaith_minority_communities: diffuse beneficiary — loses textual cover others might invoke against them
 *   - militant_political_movements: excluded — rejects the reading's premises outright, outside its discourse community
 *   - comparative_religion_academics: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.68).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.61).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, piton).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Progressive-Synthesis Reading of Verse 9:5 as Superseded Historical Directive").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '26a5e467-905e-496a-8910-baa1a4bc4a54').
narrative_ontology:cs_kernel_codification('26a5e467-905e-496a-8910-baa1a4bc4a54', fixed_text).
narrative_ontology:cs_authority_grounding('26a5e467-905e-496a-8910-baa1a4bc4a54', distributed).
narrative_ontology:cs_reading_relation('26a5e467-905e-496a-8910-baa1a4bc4a54', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('26a5e467-905e-496a-8910-baa1a4bc4a54', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('26a5e467-905e-496a-8910-baa1a4bc4a54', foundational, quranic_ethical_trajectory_supersedes_verse_literalism).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_verse_literalism, holdable).
narrative_ontology:cs_axiom_grounding('26a5e467-905e-496a-8910-baa1a4bc4a54', quranic_ethical_trajectory_supersedes_verse_literalism, conventional).
narrative_ontology:cs_axiom('26a5e467-905e-496a-8910-baa1a4bc4a54', foundational, time_bound_political_directives_carry_no_perpetual_legal_force).
narrative_ontology:cs_axiom_status(time_bound_political_directives_carry_no_perpetual_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('26a5e467-905e-496a-8910-baa1a4bc4a54', time_bound_political_directives_carry_no_perpetual_legal_force, instrumental).
narrative_ontology:cs_reference_frame('26a5e467-905e-496a-8910-baa1a4bc4a54', revelatory_context_bound_legislation).
narrative_ontology:cs_drift_state('26a5e467-905e-496a-8910-baa1a4bc4a54', post_colonial_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('26a5e467-905e-496a-8910-baa1a4bc4a54', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_legal_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, interfaith_minority_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, traditional_madrasa_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutions whose interpretive authority rests on treating verse 9:5 as retaining active legal force (whether as universal abrogating command or as narrower defensive rule). Under this reading, their entire jurisprudential claim to be transmitting binding divine law on this question loses its object — the verse has exited active constraint space. They cannot simply concede this without conceding the broader method (classical abrogation doctrine, continuous applicability of Medinan legislative verses) that underwrites much of their authority elsewhere. Exit from the interpretive commitment is barred by institutional identity, not merely inconvenient.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, civilizational, identity_locked, global).

% Trained scholars whose credentialing, career, and self-understanding are built on classical usul al-fiqh methodology, including doctrines of naskh (abrogation) that this reading treats as a category error when applied to 9:5. Adopting the progressive-synthesis reading would require repudiating training investments and reclassifying the interpretive tools that make them authoritative teachers within their tradition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, traditional_madrasa_jurists, payer,
    organized, generational, identity_locked, regional).

% Scholars and public intellectuals who advance historicist and maqasid-based (objectives-of-the-law) hermeneutics, arguing the Quran's ethical trajectory (toward justice, pluralism, gradual reform) supersedes literalist readings of time-bound verses. They author and circulate this reading, drawing on hermeneutic traditions (e.g. Fazlur Rahman's double-movement theory) to relocate 9:5 as historically situated rather than perpetually binding. Their institutional position varies by context — sometimes marginal within traditional seminaries, sometimes dominant within university religious-studies departments and progressive Muslim organizations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars, agenda_setter,
    organized, generational, mobile, global).

% Constitutional and civil legal systems in Muslim-majority and diaspora contexts that benefit when religious literalist claims to override pluralist civil law lose textual grounding. This reading removes one frequently-cited textual basis for religiously-justified coercion against non-Muslims or dissenters, easing the legitimacy burden on secular or mixed legal orders that must otherwise negotiate around competing claims of eternal scriptural mandate.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_legal_frameworks, beneficiary,
    institutional, generational, arbitrage, national).

% Religious minorities living under or adjacent to jurisdictions where 9:5 is invoked in political or vigilante contexts to justify coercion or subordination. They have no direct voice in the hermeneutic dispute but are structurally affected by which reading prevails locally — the progressive-synthesis reading, if adopted by local authorities, removes textual cover for coercive claims made against them, though they cannot personally cause that adoption.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, interfaith_minority_communities, beneficiary,
    powerless, biographical, trapped, regional).

% Groups that invoke the abrogating-universal reading to justify offensive action are not party to the academic hermeneutic contest that produces the progressive-synthesis reading and would reject its premises outright; they are excluded from this reading's discourse community entirely, not merely disagreeing within it. Their voice is absent from the scholarly conversation that authors and refines this reading, and this reading's proponents do not treat their objections as internal to the debate.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, militant_political_movements, excluded,
    organized, immediate, trapped, regional).

% Historians of religion and Quranic studies scholars who study how all three readings function institutionally and politically without being committed partisans of any single one. They document the reading's adoption patterns, its rhetorical strategies, and its political uses without needing to adjudicate its theological correctness.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, comparative_religion_academics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic framework that lets contemporary Muslim communities and institutions affirm continuity with scripture while disengaging inherited verses whose plain historical referent (specific Medinan treaty-breakers) no longer applies, avoiding the coordination failure of either wholesale scriptural rejection or literal re-enactment of 7th-century political-military directives in the present.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy away from institutions whose standing depends on classical abrogation doctrine and toward historicist scholars, reformist institutions, and pluralist legal frameworks; moves textual cover for coercive claims against religious minorities away from those who would invoke verse 9:5 politically.
% ABSENT_VOICES: Militant political movements that rely on the abrogating-universal reading for legitimacy are not represented in the scholarly conversation that produces this reading and would reject its historicist premises as themselves illegitimate innovation (bid'ah); ordinary lay believers who inherit whichever reading their local institution transmits, without direct participation in the hermeneutic contest, are also largely absent from authorship.
% DISAPPEARANCE_RATIONALE: If this reading vanished from scholarly and public discourse, traditionalist institutions would lose a major counter-framework they must currently engage or rebut, secular legal systems would lose one hermeneutic resource for resisting literalist political claims, and reform-minded believers would lose an interpretive path that lets them retain scriptural commitment without literalist consequence — proponents say the world rearranges toward unchallenged textualism; textualist critics say nothing changes because they regard this reading as having never held genuine interpretive authority to begin with.
% FOUNDING_PROBLEM: Historicist Quranic hermeneutics emerged to resolve the felt tension between scripture perceived as containing time-bound 7th-century political-military directives and the need for a coherent, non-selectively-literalist ethical framework applicable in pluralistic modern states — particularly acute for verses cited to justify warfare against non-Muslims.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars (Fazlur Rahman's double-movement theory, contemporary maqasid theorists) attest the founding problem is live and their framework resolves it. Comparative religion academics, largely outside the reformist camp itself, corroborate that the interpretive tension this reading responds to is real and documented across multiple traditions' hermeneutic histories, independent of whether they endorse this particular resolution. Traditionalist jurists dispute that the problem exists at all in the terms reformists state it, treating the entire historicist framing as a modern imposition rather than a genuine resolution of an inherited tension.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high (0.68 at interval end) because the reading, if institutionally adopted, substantially devalues the interpretive capital of textualist authority structures and traditional jurists — this is a real transfer of legitimacy and resource-access (teaching positions, fatwa authority, funding), not merely intellectual disagreement. Suppression is authored at 0.61: the reading does not physically coerce anyone, but it does actively delegitimize a rival interpretive tradition's truth-claims in public and academic discourse, and in secular-legal contexts it can be operationalized to exclude textualist claims from legal recognition — a real, if soft, suppressive effect on the losing tradition's authority. Theater ratio is moderate and rising (0.25 to 0.42) reflecting that as the reading has become institutionally entrenched in Western academic and some reformist religious contexts, a growing share of its invocation is performative signaling of progressive credentials rather than close textual-historical argument. Accessibility collapse is authored low-moderate (0.4): unlike a mountain, alternative readings remain fully articulable and actively defended — textualism has not been rendered unthinkable, only contested. Resistance is authored high (0.78): traditionalist scholarship mounts sustained, well-resourced counter-argument against this reading, which is exactly what a genuinely contested hermeneutic claim should show, not what a settled natural fact would show.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist authority structures and traditional jurists are declared victims because their institutional and professional standing is the structural object being displaced — they are identity-locked (their exit would require abandoning classical methodology that underwrites authority elsewhere in their system, not just this one verse). Reformist scholars are the agenda-setters who author and advance the reading, with mobile exit options (able to move between academic, activist, and religious institutional contexts). Secular-pluralist legal frameworks and interfaith minority communities are declared beneficiaries at very different power levels — the former an institutional beneficiary with arbitrage-grade exit (can draw on multiple hermeneutic traditions as convenient), the latter a powerless, geographically trapped population that benefits only diffusely and cannot itself cause the reading's adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is authored as piton rather than rope or tangled_rope because, from the perspective internal to this story, the *original* coordination function it responds to (resolving tension between scripture and pluralist modern governance) may itself be judged either live or dead depending on which corroborating source is consulted — reformists say the problem remains live and their synthesis actively solves it; comparative-religion academics attest the underlying interpretive tension is real but do not adjudicate whether progressive-synthesis specifically remains the operative solution versus having become a settled academic orthodoxy maintained more by institutional inertia (tenure, curriculum, ideological commitment) than active argumentative renewal in some settings. The rising theater_ratio and absence of any single concentrated profiteer (no party 'collects rent' the way a captured regulator would; the beneficiaries gain removed textual cover, not extracted revenue) point toward piton rather than snare — but the story deliberately leaves this classification epistemically contested via the mandatrophy omega below, since resolving it requires empirical work on how the reading is actually used in contemporary institutions versus how it is presented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_as_theological_or_political_act,
    'Is the adoption of the progressive-synthesis reading over its siblings best understood as a genuine hermeneutic discovery (recovering the Quran''s true ethical trajectory) or as a politically motivated accommodation to contemporary pluralist norms that retrofits scripture to fit prior commitments?',
    'Comparative analysis of whether the double-movement/maqasid methodology, applied consistently across all legislative verses (not selectively to politically inconvenient ones), produces the same historicizing result — a test of methodological consistency versus ad hoc application.',
    'If methodologically consistent, the reading has stronger claim to genuine hermeneutic status; if selectively applied only to verses that conflict with contemporary pluralist commitments, it more closely resembles motivated reasoning dressed as scholarship, strengthening the textualist critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_theological_or_political_act, conceptual, 'Whether progressive-synthesis is principled hermeneutics or selective retrofitting.').

omega_variable(
    kernel_framing_disambiguation,
    'Is the quran_9_5_scope kernel best framed as a single textual dispute with three readings (as modeled here), or does the underlying disagreement actually reduce to two deeper, more fundamental kernels — one about the doctrine of abrogation (naskh) itself, and one about whether the Quran contains a discoverable ''ethical trajectory'' independent of any specific verse''s plain sense — with 9:5 merely the flashpoint where both deeper disputes become visible?',
    'Trace whether resolving the naskh-doctrine dispute and the ethical-trajectory dispute independently (in contexts unrelated to verse 9:5) would predict each party''s position on 9:5 without reference to 9:5 itself; if so, 9:5 is downstream of two more fundamental kernels rather than being a kernel in its own right.',
    'If the deeper kernels are the real locus of disagreement, this story and its siblings are better modeled as one shared downstream node influenced by two upstream kernel families, changing the network topology substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_disambiguation, conceptual, 'Whether verse 9:5 is itself a kernel or a downstream flashpoint of deeper doctrinal kernels.').

omega_variable(
    piton_vs_tangled_rope_classification,
    'Does this reading currently function as inert academic/institutional inertia (piton) with no concentrated beneficiary capturing rents, or has it been captured by specific reformist institutions (foundations, university programs, media platforms) that derive concentrated funding and status benefits from championing it, making it closer to a tangled_rope?',
    'Trace funding flows and institutional positioning of the most prominent proponents of progressive-synthesis readings to determine whether specific organizations capture disproportionate benefit (grants, media visibility, political access) from the reading''s prominence, versus the reading being diffusely held without concentrated capture.',
    'If concentrated capture is found, reclassify from piton toward tangled_rope (genuine ethical-coordination function plus identifiable asymmetric beneficiary); if capture is genuinely diffuse, piton classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_tangled_rope_classification, empirical, 'Whether concentrated institutional capture exists behind the diffuse-appearing beneficiary structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__progressive_synthesis, theater_ratio, 10, 0.29).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__progressive_synthesis, theater_ratio, 20, 0.33).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__progressive_synthesis, theater_ratio, 30, 0.36).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__progressive_synthesis, theater_ratio, 40, 0.38).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__progressive_synthesis, theater_ratio, 50, 0.4).
narrative_ontology:measurement(qura_tr_t60, quran_9_5_scope__progressive_synthesis, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__progressive_synthesis, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__progressive_synthesis, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__progressive_synthesis, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__progressive_synthesis, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__progressive_synthesis, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(qura_be_t60, quran_9_5_scope__progressive_synthesis, base_extractiveness, 60, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_9_5_scope__progressive_synthesis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the scope of Quran 9:5' per the ε-invariance principle. abrogating_universal claims a standing universal legal obligation (high ε, victims: polytheist/non-Muslim populations and treaty-partners under threat of coercion). contextual_defensive claims a narrow, non-abrogating, treaty-context-bound rule (lower ε, victims limited to actual treaty-breakers historically). progressive_synthesis (this story) claims the verse has exited active constraint space entirely, and its measured extraction runs instead against textualist authority structures whose institutional legitimacy depends on one of the other two readings remaining true. The three readings do not share an ε value — they are not the same constraint measured three ways, they are three constraints sharing one textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
