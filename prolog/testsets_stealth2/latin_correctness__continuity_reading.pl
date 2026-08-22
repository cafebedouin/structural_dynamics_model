% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Continuity Reading: Medieval Latin as Organic Continuation of Classical Latin
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   A scholarly normative arrangement treats medieval Latin — the Latin of
 *   charters, liturgy, theology, science, and letters from roughly the sixth
 *   century onward — as the same language as classical Latin, changed by the
 *   ordinary drift any long-lived language undergoes. On this reading,
 *   medieval spellings that mirror contemporary pronunciation, thousands of
 *   newly coined words for institutions and artifacts Rome never knew, and
 *   reshaped syntax are evidence of a living written tradition, not lapses
 *   from a norm. The arrangement is carried by editorial house styles that
 *   reproduce manuscript orthography, by lexica that admit medieval
 *   vocabulary on equal footing, and by didactic practice that teaches
 *   medieval texts as Latin outright. Its costs are light and diffuse:
 *   students must master variable rather than single norms, and editors must
 *   document rather than silently repair. No party bears concentrated
 *   burdens, and no party is barred from anything by it. KEY AGENTS (by
 *   structural relationship): - medieval_manuscript_philologists: Primary
 *   beneficiary (organized/constrained) — gains full interpretive legitimacy
 *   for their corpus - paleography_diplomatics_community: Beneficiary
 *   (organized/constrained) — evolutionary dating presupposes continuous
 *   change - romance_historical_linguists: Secondary beneficiary
 *   (organized/mobile) — uses the continuum as one anchor among several -
 *   medieval_edition_series_boards: Agenda-setter (institutional/mobile) —
 *   administers editorial norms, draws incidental standing -
 *   classicist_purists: Excluded voice (powerful/constrained) — objection
 *   seated in the sibling readings, not here - philology_historians:
 *   Analytical observer — sees the full structure across readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.1).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.15).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Continuity Reading: Medieval Latin as Organic Continuation of Classical Latin").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, 'bd6ec54d-e4d4-40bd-8019-e514e45333a4').
narrative_ontology:cs_kernel_codification('bd6ec54d-e4d4-40bd-8019-e514e45333a4', distributed).
narrative_ontology:cs_authority_grounding('bd6ec54d-e4d4-40bd-8019-e514e45333a4', expertise).
narrative_ontology:cs_interpretation_layer_present('bd6ec54d-e4d4-40bd-8019-e514e45333a4').
narrative_ontology:cs_reading_relation('bd6ec54d-e4d4-40bd-8019-e514e45333a4', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('bd6ec54d-e4d4-40bd-8019-e514e45333a4', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('bd6ec54d-e4d4-40bd-8019-e514e45333a4', foundational, organic_change_preserves_legitimacy).
narrative_ontology:cs_axiom_status(organic_change_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bd6ec54d-e4d4-40bd-8019-e514e45333a4', organic_change_preserves_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('bd6ec54d-e4d4-40bd-8019-e514e45333a4', foundational, no_trans_temporal_normative_standard).
narrative_ontology:cs_axiom_status(no_trans_temporal_normative_standard, holdable).
narrative_ontology:cs_axiom_grounding('bd6ec54d-e4d4-40bd-8019-e514e45333a4', no_trans_temporal_normative_standard, conventional).
narrative_ontology:cs_axiom('bd6ec54d-e4d4-40bd-8019-e514e45333a4', secondary, synchronic_variance_as_evidence).
narrative_ontology:cs_axiom_status(synchronic_variance_as_evidence, holdable).
narrative_ontology:cs_axiom_grounding('bd6ec54d-e4d4-40bd-8019-e514e45333a4', synchronic_variance_as_evidence, instrumental).
narrative_ontology:cs_reference_frame('bd6ec54d-e4d4-40bd-8019-e514e45333a4', unbroken_transmission_continuum).
narrative_ontology:cs_drift_state('bd6ec54d-e4d4-40bd-8019-e514e45333a4', contemporary_diglossia_scholarship, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('bd6ec54d-e4d4-40bd-8019-e514e45333a4', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_manuscript_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, paleography_diplomatics_community).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, romance_historical_linguists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_edition_series_boards).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, diachronic_continuity_of_written_latin).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, internal_evidence_dating_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit, translate, and interpret the surviving corpus of medieval Latin writing — charters, theology, poetry, letters, scientific treatises. Their working assumption is that a text's non-classical spellings, word forms, and syntax are data about its time and place rather than faults to be repaired; editions record variance instead of normalizing it. Their methods of internal dating, attribution, and dialect localization presuppose that forms evolved along traceable lines. Leaving the framework would mean discarding the interpretive instruments their careers are built on.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_manuscript_philologists, beneficiary,
    organized, generational, constrained, continental).

% Date and authenticate manuscripts and documents from letterforms, abbreviations, scripts, and formulaic language. Their craft ranks specimens along evolutionary sequences — Caroline minuscule to Gothic, formula drift in charters — which yields chronological signal only if the written language changed continuously and lawfully. External checks exist in dated colophons and regnal years, but the internal sequence is the daily instrument. Abandoning the continuity premise would reduce them to external dating alone.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, paleography_diplomatics_community, beneficiary,
    organized, generational, constrained, continental).

% Reconstruct how Latin turned into French, Spanish, Italian, and their kin. They read late-antique and medieval Latin texts as upstream stations on the route to the Romance languages, using medieval spellings that mirror pronunciation as evidence of sound change. Their field would survive on Romance-internal evidence alone, but the Latin-side continuum multiplies their data and anchors reconstructions; they can and do work from either side.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, romance_historical_linguists, beneficiary,
    organized, generational, mobile, continental).

% Editorial boards of the great medieval source series and lexicon projects commission volumes, set house style, and fund lexicographic labor. House style instructs editors to reproduce manuscript orthography, document variants in apparatus, and admit medieval vocabulary in dictionaries on equal footing rather than exclude it as barbarism. The boards could adopt classicizing norms, but their reviewer networks, completed volumes, and subscriber expectations are built on current conventions, so change would be slow and fractious. They administer the arrangement more than they profit from it.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_edition_series_boards, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, medieval_edition_series_boards, beneficiary).

% Scholars trained primarily on ancient texts who judge post-classical writing against the grammar and style recovered from Cicero and his contemporaries. They publish in classics venues, teach classical authors, and regard much medieval prose as degenerate or as a different thing altogether. Their objections carry weight in classics departments and in aesthetic judgment of literary quality, but they do not sit on the boards or in the seminars that set practice for medieval corpora; their position is developed in its own right elsewhere.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classicist_purists, excluded,
    powerful, generational, constrained, global).

% Study how the disciplines that read old texts were built — how Renaissance humanists, seventeenth-century lexicographers, and nineteenth-century editorial academies shaped what counts as good Latin and why. They take no side in the substantive dispute; they map who held which position, when, and with what consequences for editions and curricula.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, philology_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the scattered communities that handle medieval Latin texts a shared diachronic standard: one answer to the question 'is this form an error or evidence?' that lets editors, paleographers, lexicographers, and linguists pool results across a millennium of manuscripts without a living normative authority to appeal to.
% TRANSFER_FUNCTION: Moves methodological authority and standing rather than goods: legitimacy flows from the classical-norm arbiters to the medieval texts themselves and to the specialists equipped to read them on internal evidence; editorial labor shifts from silent repair toward documentation. No money or material resource moves through the arrangement.
% ABSENT_VOICES: Rupture- and hybrid-leaning classicists would object that continuity licensing erodes the fixed standard by which literary quality and doctrinal precision are judged; they are seated in the sibling stories rather than here, and hold their own venues meanwhile. More fundamentally, the parties whose legitimacy is adjudicated — the medieval writers themselves — are dead and cannot testify; their voice exists only as reconstructed through the very framework under assessment, which is why corroboration is sought in external dating and comparative evidence rather than testimony.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, every evolutionary dating sequence in paleography and diplomatics would lose its premise, a century of editions documenting rather than normalizing variance would lose their rationale, Romance reconstruction would lose its Latin-side anchor, and lexicography would stall mid-letter; editorial practice would revert to classicizing repair and the medieval corpus would become readable only as a quarry of faults. The world of medieval text scholarship rearranges around the loss.
% FOUNDING_PROBLEM: After the humanist dismissal of medieval Latinity, scholars confronting the enormous unpublished corpus of charters, conciliar acts, sermons, and treatises had no usable standard: the texts obeyed no living native authority, diverged widely from ancient grammar, and yet had to be read, dated, and edited reliably. The continuity arrangement was built to solve that: take the language's own development as the standard, and the corpus becomes datable, localizable, and editable on internal evidence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: charter archives supply externally fixed dates — regnal years, dated colophons — against which internally derived linguistic datings are checked, so archival institutions rather than medievalists vouch for the targets; Romance-language reconstruction, pursued from the daughter languages' side, independently attests continuous change across the same centuries; and classicists themselves concede unbroken development through late antiquity, conceding the mechanism if not the medieval extension. Medievalists' own testimony is deliberately not counted here.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is rope because the arrangement solves a genuine collective problem — interpreting a millennium of writing produced under no living normative authority — with net beneficiaries, no identifiable victim set, and no coercive overhead. The metrics describe its actual operation: extractiveness 0.10 because the only costs are diffuse and light (pedagogical complexity, loss of a single yardstick); suppression 0.15 because nothing enforces the reading — dissenting classicists publish, teach, and hire unpunished, and suppression here is a raw structural property left unscaled; theater_ratio 0.12 because the arrangement's work is overwhelmingly operational (dating, editing, lexicography) with only a shallow residue of rhetorical sloganizing ('Latin never died') in textbooks; accessibility_collapse 0.30 because the rival readings remain fully articulable and practiced — understanding the continuity position does not collapse them, which is precisely why this is not a natural-law candidate; resistance 0.20 reflecting residual purist pushback concentrated at boundary cases of literary quality. The temporal series run on one shared grid (points 0–100, roughly 1920–2020) with every tracked metric authored at every point; suppression_requirement is intentionally not tracked because the enforcement picture is static — the arrangement runs on consensus, not enforcement capacity, so the scalar suppression value already carries the whole story.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading the seats align near the beneficiary pole: every declared party either collects interpretive capability or administers without taking a toll, so per-seat computed types should come out near-uniformly benign. The interesting divergence is therefore not between seats inside this story but between readings over the same historical material: the rupture_reading file assigns the same medieval usage a high epsilon and a victim structure (readers misled, the classical standard itself profaned), while this file assigns near-zero epsilon and no victims. The engine's per-seat computation on this story should show little internal spread; the perspectival signal lives in cross-file comparison across the kernel family.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiary groups derive low directionality (near the full-beneficiary end): the arrangement subsidizes their interpretive practice at no charge. The edition boards, as agenda_setters with a secondary beneficiary position, derive near-symmetric directionality — they run the arrangement and draw incidental standing from it, but collect no toll from outsiders. The excluded classicist seat carries no directionality in this story: nothing in the continuity arrangement takes anything from them, and their grievance is structurally measured in the sibling files where it is constitutive. With no declared victims and no enforcement machinery, aggregate effective extraction stays near the floor; the residual 0.10 reflects real but light diffuse costs, not targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to edit and interpret a millennium of writing produced under no living native normative authority — is still live, so no mandatrophy is declared. The classification guards both mislabeling directions: against reading the arrangement as coordination ideology masking extraction (there are no victims to hide), and against a premature piton verdict — the prohibitive-fixing/diffuse-gain profile recorded on the receipt surface here belongs to a working consensus whose function is intact, not to an atrophied shell, because theater stays low and the dating instruments still perform. The drift worth watching is credentialing: if professing continuity ever became a ticket to publication independent of its evidential work, theater and extractiveness would climb together; the measurement series tracks exactly that signature and currently shows only a shallow slope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the latin_correctness kernel; the sibling readings (rupture_reading, hybrid_reading) instantiate different constraints over the same textual material — where exactly does the disagreement between readings bite?',
    'Compare the sibling files'' authored epsilon values, victim structures, and coordination types; the disagreement is located in the indexing of correctness (fixed textual standard vs. domain partition vs. organic transmission), and cross-file comparison of computed per-seat classifications localizes which structural element drives the divergence.',
    'If the hybrid reading proves descriptively dominant in actual editorial practice, part of this reading''s coordination function is absorbed by the hybrid arrangement and this story''s effective extraction rises slightly through boundary-policing costs; if the rupture reading regains institutional ground, this story''s beneficiary set shrinks correspondingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints, not parts of this one.').

omega_variable(
    organicity_of_learned_register,
    'Is ''organic linguistic change'' an accurate description of medieval Latin''s development, given that it was a learned register coexisting with the Romance vernaculars in a diglossic situation, or is ''organic'' a naturalizing metaphor for what was partly scribal fashion?',
    'Compare medieval Latin''s innovation patterns against (a) native-speech communities and (b) other learned registers (Byzantine Greek, Islamicate Arabic); test whether lexical and phonological innovations track contemporaneous speech or track chancery and school fashions.',
    'If innovations are largely scribal-fashion-driven, the continuity frame''s mechanism claim weakens and the drift_state''s axiom_overriding deepens, while the legitimacy conclusion may survive on transmission-continuity grounds alone; if they track speech, the organic description is vindicated and the reading''s epistemic standing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organicity_of_learned_register, empirical, 'Whether the ''organic change'' mechanism claim survives the diglossia correction.').

omega_variable(
    silent_adjudicated_parties,
    'The parties whose legitimacy this reading adjudicates — the medieval writers themselves — cannot testify, and every declared beneficiary is a present-day interpreter: is the reading''s warrant evidential (the texts corroborate continuity) or professional self-interest (whole careers are built on the framework)?',
    'Blind tests of dating and localization predictions drawn from internal linguistic evidence against externally fixed dates (regnal-year charters, dated colophons, archaeologically anchored finds); a high predictive success rate distinguishes evidential warrant from circular professional consensus.',
    'Strong predictive performance secures the reading''s standing independent of beneficiary interest and stabilizes the rope classification; systematic failure would shift weight toward the rupture and hybrid framings and convert this story''s beneficiaries into parties defending an unfalsifiable credential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silent_adjudicated_parties, empirical, 'Evidential versus professional warrant for a reading whose adjudicated parties cannot speak.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_continuity_tr_t0, latin_correctness__continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(latin_continuity_tr_t0, observed).
narrative_ontology:measurement(latin_continuity_tr_t20, latin_correctness__continuity_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(latin_continuity_tr_t20, observed).
narrative_ontology:measurement(latin_continuity_tr_t40, latin_correctness__continuity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(latin_continuity_tr_t40, observed).
narrative_ontology:measurement(latin_continuity_tr_t60, latin_correctness__continuity_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement_basis(latin_continuity_tr_t60, observed).
narrative_ontology:measurement(latin_continuity_tr_t80, latin_correctness__continuity_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(latin_continuity_tr_t80, observed).
narrative_ontology:measurement(latin_continuity_tr_t100, latin_correctness__continuity_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(latin_continuity_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(latin_continuity_be_t0, latin_correctness__continuity_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement_basis(latin_continuity_be_t0, observed).
narrative_ontology:measurement(latin_continuity_be_t20, latin_correctness__continuity_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(latin_continuity_be_t20, observed).
narrative_ontology:measurement(latin_continuity_be_t40, latin_correctness__continuity_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(latin_continuity_be_t40, observed).
narrative_ontology:measurement(latin_continuity_be_t60, latin_correctness__continuity_reading, base_extractiveness, 60, 0.09).
narrative_ontology:measurement_basis(latin_continuity_be_t60, observed).
narrative_ontology:measurement(latin_continuity_be_t80, latin_correctness__continuity_reading, base_extractiveness, 80, 0.1).
narrative_ontology:measurement_basis(latin_continuity_be_t80, observed).
narrative_ontology:measurement(latin_continuity_be_t100, latin_correctness__continuity_reading, base_extractiveness, 100, 0.1).
narrative_ontology:measurement_basis(latin_continuity_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'is medieval Latin correct Latin?' decomposes into three structurally distinct constraints — the continuity, rupture, and hybrid readings of kernel latin_correctness — each with its own epsilon referent, beneficiary/victim structure, and coordination profile; this file is the continuity member. Edges run to both siblings because the same body of texts is the contested material: whichever reading governs editorial practice determines what the others can even measure. Genealogically the rupture reading is upstream (the humanist fixed standard that the continuity arrangement answered), and the hybrid reading sits downstream of both as a partition compromise. Per the epsilon-invariance principle, no single story averages across the readings; the family is linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
